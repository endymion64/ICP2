             /*-----------------------------------*/
             /*          >>>PicoShell<<<          */
             /*            Theo D'Hondt           */
             /*   VUB Programming Technology Lab  */
             /*             (c) 2002              */
             /*-----------------------------------*/
             /*       PicoShell source file       */
             /*-----------------------------------*/
             /* Adapted somewhat by Dirk van Deun */
             /*         for wxPico (2004)         */
             /*-----------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../Pico/Pico.h"
#include "../Pico/PicoMai.h"
#include "PicoStrings.h"
#include "PicoShell.h"

/* private constants */

#define INTERRUPT_STR          "\ninterrupted\n"  
#define RETURN_STR             "\n"
#define RETURN_CHAR            '\n'
#define LEFT_PARENTHESIS_CHAR  '('
#define RIGHT_PARENTHESIS_CHAR ')'
#define LEFT_BRACKET_CHAR      '['
#define RIGHT_BRACKET_CHAR     ']'
#define LEFT_BRACE_CHAR        '{'
#define RIGHT_BRACE_CHAR       '}'
#define CANNOT_OPEN_LOAD_FILE  "Cannot open load file"
#define SHELL_IS_INITIALIZED   "Shell already initialized"
#define SHELL_NOT_INITIALIZED  "Shell not initialized"
#define SOME_SHELLS_ARE_BUSY   "Some shells are busy"
#define TOO_MANY_SHELLS        "Too many shells"
#define ILLEGAL_SHELL          "Illegal shell"
#define SHELL_NOT_IN_USE       "Shell not in use"
#define BUFFER_OVERFLOW        "Buffer overflow"
#define MESSAGE_OVERFLOW       "Message overflow"
#define STACK_OVERFLOW         "Stack overflow"
#define BALANCE_CONFLICT       "Balance conflict"
#define BUFFER_SIZE            32768
#define MESSAGE_SIZE           256
#define MAX_SHELLS             _MAX_SES_ 

#define FALSE             0
#define TRUE              1

#define FREE              0
#define IDLE              1
#define WAITING           2 
#define ENTERING          3
#define ACCEPTING         4
#define BUSY              5

#define PARENTHESES 0
#define BRACKETS    1
#define BRACES      2

#define PICO_DONE_TEXT "done"

/* private variables */

static _ADR_TYPE_ storage_address = NULL;
static _SIZ_TYPE_ storage_size;
static unsigned   Count;
static unsigned   Current;
static unsigned   NextFree;
static char       Buffer[BUFFER_SIZE];
static struct     { void *     window;
		                char       msg[MESSAGE_SIZE];
		                long       begin;
		                short      state;
		                short      interrupt;
		                short      kill;
		                short      newline; } Shell[MAX_SHELLS];

/* private functions */

static short valid_index(const unsigned index)
  { if(index >= MAX_SHELLS)
      { _SHELL_ERROR_(ILLEGAL_SHELL); 
        return FALSE; }
    else if (index >= NextFree)
      { _SHELL_ERROR_(SHELL_NOT_IN_USE);
        return FALSE; }
    else if (Shell[index].state == FREE)
      { _SHELL_ERROR_(SHELL_NOT_IN_USE);
        return FALSE; }
    else
      return TRUE; }
      
static void set_shell(const unsigned index  , 
                      const short    state  , 
                      const short    newline)
  { Shell[index].msg[0]      = 0;
    Shell[index].begin       = 0; 
    Shell[index].state       = state;
    Shell[index].interrupt   = FALSE;
    Shell[index].kill        = FALSE;
    Shell[index].newline     = newline; }

static void doProceed(const unsigned   index, 
                      const _RES_TYPE_ res  )
  { void * window = Shell[index].window;
    switch (res)
      { case _DO_ESCAPE_:
          if(Shell[index].interrupt)
            { Shell[index].interrupt = FALSE;
              Count--; 
              set_shell(index, IDLE, TRUE);
              _SHELL_PRINT_STRING_(window, INTERRUPT_STR);
              return; }
          if(Shell[index].kill)
					  { _RES_TYPE_ res;
			        Count--; 
			        res = _PICO_KILL_((_SES_TYPE_)index); 
			        if(res != _PICO_DONE_)     
			          _SHELL_ERROR_IN_WINDOW_(PICO_ERROR(res),window);
			        set_shell(index, FREE, TRUE); }
          return;
        case _DO_ACCEPT_: 
          Count--;
          Shell[index].state = ENTERING;
          Shell[index].begin = _SHELL_TEXT_LENGTH_(window);
          _SHELL_ACCEPT_PROMPT_(window);
          return;
        case _DO_LOAD_: 
          _SHELL_LOAD_FILE_(Shell[index].msg, Buffer, Shell[index].window); 
          Shell[index].msg[0] = 0;
          doProceed(index, _PICO_LOAD_((_SES_TYPE_)index, Buffer)); 
          return;                
        case _PICO_DONE_:
          Count--; 
          set_shell(index, IDLE, TRUE);
          _SHELL_PRINT_STRING_(window, RETURN_STR);
          return;
        default:
          Count--; 
          if (Shell[index].msg[0])
            { sprintf(Buffer, "%s: %s", PICO_ERROR(res), Shell[index].msg);
              Shell[index].msg[0] = 0;
              _SHELL_ERROR_IN_WINDOW_(Buffer,window); }
          else
            _SHELL_ERROR_IN_WINDOW_(PICO_ERROR(res),window);
          set_shell(index, IDLE, TRUE); }}
          
static void doEvaluate(const unsigned index)
  { void *   window = Shell[index].window;
	  unsigned length;
	  unsigned offset;
	  unsigned start;
	  unsigned stop;
    _SHELL_GET_SELECTION_(window, &start, &stop);
    if(start >= stop)
      { if(stop > BUFFER_SIZE - 1)
          goto overflow;
        _SHELL_COPY_SELECTION_(window, 0, stop, Buffer);
        start = stop;
        while ((start > 0) &&
               (Buffer[start - 1] != RETURN_CHAR))
          start--;
        offset = start; }
    else
      { length = stop - start;
        offset = 0;
        if(length > BUFFER_SIZE - 1)
          goto overflow;
        _SHELL_COPY_SELECTION_(window, start, stop, Buffer); }
    Shell[index].state = BUSY;
    Shell[index].begin = start;
    doProceed(index, _PICO_DO_((_SES_TYPE_)index, Buffer + offset));
    return;    
  overflow:
    Count--; 
    set_shell(index, IDLE, TRUE);
    _SHELL_ERROR_IN_WINDOW_(BUFFER_OVERFLOW,window); }


static void doAccept(const unsigned index)
  { void *   window = Shell[index].window;
	  unsigned start;
	  unsigned stop;
	  unsigned length;
    start = Shell[index].begin;
    stop = _SHELL_TEXT_LENGTH_(window);
    length = stop - start;
    if(length < BUFFER_SIZE-1)
      { _SHELL_COPY_SELECTION_(window, start, stop, Buffer);
        Shell[index].state = BUSY;
        doProceed(index, _PICO_ACCEPT_((_SES_TYPE_)index, Buffer)); }
    else
      { Count--;
        set_shell(index, IDLE, TRUE);
        _SHELL_ERROR_IN_WINDOW_(BUFFER_OVERFLOW,window); }}

static void sweep_left(unsigned index    ,
                       int      left     , 
                       int      right    , 
                       short    delimiter)
  { int current;
    short stack[256];
    short count;
	  count = 0;
	  current = left;
    while (current > 0)
      { switch(Buffer[--current])
	        { case RIGHT_PARENTHESIS_CHAR: 
	            stack[count++] = (PARENTHESES);
	            break;
	          case RIGHT_BRACKET_CHAR: 
	            stack[count++] = (BRACKETS);
	            break;
	          case RIGHT_BRACE_CHAR: 
	            stack[count++] = (BRACES);
	            break;
	          case LEFT_PARENTHESIS_CHAR:
	            if(count == 0)
	              { _SHELL_SET_SELECTION_(Shell[index].window, current, right);
	                if(delimiter != (PARENTHESES))
	                  _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            if(stack[--count] != PARENTHESES)
	              { _SHELL_SET_SELECTION_(Shell[index].window, current, right);
	                _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            break;
	          case LEFT_BRACKET_CHAR:
	            if(count == 0)
	              { _SHELL_SET_SELECTION_(Shell[index].window, current, right);
	                if(delimiter != (BRACKETS))
	                  _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            if(stack[--count] != BRACKETS)
	              { _SHELL_SET_SELECTION_(Shell[index].window, current, right);
	                _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            break;
	          case LEFT_BRACE_CHAR:
	            if(count == 0)
	              { _SHELL_SET_SELECTION_(Shell[index].window, current, right);
	                if(delimiter != (BRACES))
	                  _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            if(stack[--count] != BRACES)
	              { _SHELL_SET_SELECTION_(Shell[index].window, current, right);
	                _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            break; }
	      if(count >= 256)
	        { _SHELL_ERROR_(STACK_OVERFLOW);
	          return; }}}

static void sweep_right(unsigned index , 
                        int      left  , 
                        int      right , 
                        int      length)
  { int current;
    short stack[256];
    short count;
	  count = 0;
	  current = right;
    while (current < length)
      { switch(Buffer[current++])
	        { case LEFT_PARENTHESIS_CHAR: 
	            stack[count++] = (PARENTHESES);
	            break;
	          case LEFT_BRACKET_CHAR: 
	            stack[count++] = (BRACKETS);
	            break;
	          case LEFT_BRACE_CHAR: 
	            stack[count++] = (BRACES);
	            break;
	          case RIGHT_PARENTHESIS_CHAR:
	            if(count == 0)
	              { sweep_left(index, left, current, (PARENTHESES));
	                return; }
	            if(stack[--count] != PARENTHESES)
	              { _SHELL_SET_SELECTION_(Shell[index].window, left, current);
	                _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            break;
	          case RIGHT_BRACKET_CHAR:
	            if(count == 0)
	              { sweep_left(index, left, current, (BRACKETS));
	                return; }
	            if(stack[--count] != (BRACKETS))
	              { _SHELL_SET_SELECTION_(Shell[index].window, left, current);
	                _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            break;
	          case RIGHT_BRACE_CHAR:
	            if(count == 0)
	              { sweep_left(index, left, current, (BRACES));
	                return; }
	            if(stack[--count] != (BRACES))
	              { _SHELL_SET_SELECTION_(Shell[index].window, left, current);
	                _SHELL_ERROR_(BALANCE_CONFLICT);
	                return; }
	            break; }
	      if(count >= 256)
	        { _SHELL_ERROR_(STACK_OVERFLOW);
	          return; }}}
    
/* callback functions */

_ESC_TYPE_ _PICO_ESCAPE_(const _SES_TYPE_ ses)
  { unsigned index = (unsigned)ses;
    _SHELL_EVENTS_();
    if(Shell[index].interrupt | Shell[index].kill | (Count > 1))
      { return _ESCAPE_YES_; }
    else
      return _ESCAPE_NO_; }

_ESC_TYPE_ _PICO_PRINT_(const _SES_TYPE_ ses, 
                        const _STR_TYPE_ str)
  { unsigned index  = (unsigned)ses;
    void *   window = Shell[index].window;
    if (Shell[index].newline)
      { _SHELL_PRINT_STRING_(window, RETURN_STR);
        Shell[index].newline = FALSE; }
    _SHELL_PRINT_STRING_(window, str);
    return _ESCAPE_NO_; }
   
_ESC_TYPE_ _PICO_DISPLAY_(const _SES_TYPE_ ses, 
                          const _STR_TYPE_ str)
  { unsigned index  = (unsigned)ses;
    void *   window = Shell[index].window;
    if (Shell[index].newline)
      { _SHELL_DISPLAY_STRING_(window, RETURN_STR);
        Shell[index].newline = FALSE; }
    _SHELL_DISPLAY_STRING_(window, str);
    return _PICO_ESCAPE_(ses); }
      
_NIL_TYPE_ _PICO_MARK_(const _SES_TYPE_ ses, 
                       const _POS_TYPE_ start, 
                       const _POS_TYPE_ stop)
  { unsigned index = (unsigned)ses;
    unsigned begin = Shell[index].begin;
    _SHELL_MARK_SELECTION_(Shell[index].window, begin+start, begin+stop); }

_NIL_TYPE_ _PICO_MESSAGE_(const _SES_TYPE_ ses, 
                          const _STR_TYPE_ str)
	{ unsigned index = (unsigned)ses;
	  if(strlen(str) >= MESSAGE_SIZE)
	    _SHELL_ERROR_IN_WINDOW_(MESSAGE_OVERFLOW, Shell[index].window);
	  else
      strcpy(Shell[index].msg, str); }
   
_NIL_TYPE_ _PICO_RECLAIM_(const _RCL_TYPE_ rcl)
	{ _SHELL_RECLAIM_ALERT_(); }

/* public functions */

void _SHELL_VERSION_(char * String)
  { strcpy(String, PICO_VERSION); }

void _SHELL_START_(const void *   Address,
                   const unsigned Size   )
	{ _RES_TYPE_ res;
	  if (storage_address != NULL)
	    _SHELL_ERROR_(SHELL_IS_INITIALIZED);
	  else
	    { storage_address = (_ADR_TYPE_)Address;
    	  storage_size = (_SIZ_TYPE_)Size;
     	  Count = 0;
    	  Current = MAX_SHELLS;
    	  NextFree = 0;
   		  res = _PICO_INIT_(storage_address, storage_size);
    	  if (res != _PICO_DONE_)
    			{ storage_address = NULL;
    			  _SHELL_ERROR_(PICO_ERROR(res)); }}}

void _SHELL_STOP_(void)
	{ if (storage_address == NULL)
	    _SHELL_ERROR_(SHELL_NOT_INITIALIZED);
	  else
	    storage_address = NULL; }
		
void _SHELL_RESET_(void)
	{ _RES_TYPE_ res;
	  unsigned   index;
    if (storage_address == NULL)
	    _SHELL_ERROR_(SHELL_NOT_INITIALIZED);
	  else
	    { for(index = 0; index < NextFree; index++)
          if(Shell[index].state == BUSY)
						{ _SHELL_ERROR_(SOME_SHELLS_ARE_BUSY); 
			    	  return; }
			  Count = 0;
    	  Current = MAX_SHELLS;
    	  res = _PICO_INIT_(storage_address, storage_size);
    	  if (res != _PICO_DONE_)
    			{ storage_address = NULL;
    			  _SHELL_ERROR_(PICO_ERROR(res)); }
    	  else
    	    for(index = 0; index < NextFree; index++)
	          if(Shell[index].window != FREE)
							{ res = _PICO_SESSION_((_SES_TYPE_)index);
			          if(res != _PICO_DONE_)     
				    			{ storage_address = NULL;
				    			  _SHELL_ERROR_(PICO_ERROR(res)); 
				    			  return; }
			          set_shell(index, IDLE, TRUE); }}}
		
_SHELL_ _SHELL_MAKE_(const void * window)
  { _RES_TYPE_ res;
	  unsigned   index;
	  for(index = 0; index < NextFree; index++)
	    if(Shell[index].state == FREE)
	      goto found;
	  if(NextFree >= MAX_SHELLS)
	    { _SHELL_ERROR_(TOO_MANY_SHELLS);
        return (_SHELL_)MAX_SHELLS; }
    index = NextFree++;
  found:
    res = _PICO_SESSION_((_SES_TYPE_)index);
    if(res != _PICO_DONE_)     
      { _SHELL_ERROR_(PICO_ERROR(res)); 
        return (_SHELL_)MAX_SHELLS; }
    Shell[index].window = (void *)window;
	  set_shell(index, IDLE, TRUE);   
    return (_SHELL_)index; }

void _SHELL_FREE_(const _SHELL_ shell)
  { unsigned   index = (unsigned)shell;
    _RES_TYPE_ res;
	  void *     window;
    if(valid_index(index))
      { window = Shell[index].window;
        if(Shell[index].state == BUSY)
          { Shell[index].kill = TRUE; 
            return; }
        res = _PICO_KILL_((_SES_TYPE_)index); 
        if(res != _PICO_DONE_)     
          _SHELL_ERROR_(PICO_ERROR(res));
        Shell[index].state = FREE; }}
                  
void _SHELL_PROCEED_(void)
  { if (Count > 0)
      for(;;)
        { if(++Current >= NextFree)
            Current = 0; 
          switch (Shell[Current].state)
						{ case FREE:
						    continue;
              case IDLE:
						    continue;
              case WAITING: 
                doEvaluate(Current); 
                return;
              case ENTERING:
						    return;
              case ACCEPTING:
						    doAccept(Current);
						    return;
              case BUSY:
                doProceed(Current, _PICO_CONTINUE_((_SES_TYPE_)Current)); 
                return; }}
    _SHELL_EVENTS_(); }
                
void _SHELL_EVAL_(const _SHELL_ shell)
  { unsigned index = (unsigned)shell;
    if(valid_index(index))
      switch (Shell[index].state)
				{ case FREE:
				    _SHELL_ERROR_(SHELL_NOT_IN_USE);
            return;
				  case IDLE:
				    Count++;
			      Shell[index].state = WAITING; 
			      return;
          case WAITING:
            _SHELL_BUSY_ALERT_();      
			      return;
          case ENTERING:
            _SHELL_BUSY_ALERT_();  	  
			      return;
          case ACCEPTING:	
            _SHELL_BUSY_ALERT_();    
			      return;
          case BUSY: 
            _SHELL_BUSY_ALERT_();  
            return; }}

void _SHELL_ENTER_(const _SHELL_ shell) 
  { unsigned index = (unsigned)shell;
    if(valid_index(index))
      switch (Shell[index].state)
				{ case IDLE:
				    Count++;
			      Shell[index].state = WAITING; 
			      return;
          case WAITING:
            _SHELL_BUSY_ALERT_();  
			      return;
          case ENTERING:
				    Count++;
			      Shell[index].state = ACCEPTING; 
			      return;
          case ACCEPTING:
            _SHELL_BUSY_ALERT_();  	  
			      return;
          case BUSY:
            _SHELL_BUSY_ALERT_();  
            return; }}

void _SHELL_LOAD_(const _SHELL_  shell, 
                  const char *  name)
  { unsigned index = (unsigned)shell;
    if(valid_index(index))
      { Count++;
        set_shell(index, BUSY, FALSE);
        _SHELL_LOAD_FILE_(name, Buffer, Shell[index].window); 
        doProceed(index, _PICO_DO_((_SES_TYPE_)index, Buffer)); }}

void _SHELL_INTERRUPT_(const _SHELL_ shell)
  { unsigned index = (unsigned)shell;
    if(valid_index(index))
      Shell[index].interrupt = TRUE; }
      
short _SHELL_BUSY_(const _SHELL_ shell)
  { unsigned index = (unsigned)shell;
    if(valid_index(index))
      return Shell[index].state == BUSY;
    else
      return FALSE; }
      
short _SHELL_ENTERING_(const _SHELL_ shell)
  { unsigned index = (unsigned)shell;
    if(valid_index(index))
      return Shell[index].state == ENTERING;
    else
      return FALSE; }

short _SHELL_COUNT_()
  { return Count; }

void _SHELL_BALANCE_(const _SHELL_ shell)
  { unsigned index = (unsigned)shell;
    unsigned start;
    unsigned stop;
    void *   window;
    unsigned length;
    if(valid_index(index))
      { window = Shell[index].window;
        length = _SHELL_TEXT_LENGTH_(window) + 1;
        if(length < BUFFER_SIZE)
          { _SHELL_COPY_SELECTION_(window, 0, length-1, Buffer);
		        _SHELL_GET_SELECTION_(window, &start, &stop);
		        sweep_right(index, start, stop, length); }    
        else
          _SHELL_ERROR_(BUFFER_OVERFLOW); }}    

short _SHELL_FIND_(const _SHELL_  shell,
                   const unsigned offset,
                   const char *   string)
  { unsigned index = (unsigned)shell;
    void *   window;
    unsigned cnt, length, size;
    char *   text;
    if(valid_index(index))
      { window = Shell[index].window;
        length = _SHELL_TEXT_LENGTH_(window);
        size = strlen(string);
        if(length - offset <= BUFFER_SIZE)
          { _SHELL_COPY_SELECTION_(window, offset, length-1, Buffer); 
            text = Buffer;
            for(cnt = offset; cnt < length-size; cnt++)
              if(strncmp(string,text++,size) == 0)
                { _SHELL_SET_SELECTION_(window, cnt, cnt+size);
                  return TRUE; }
            return FALSE; }
        else
          _SHELL_ERROR_(BUFFER_OVERFLOW); } 
    return FALSE; }
