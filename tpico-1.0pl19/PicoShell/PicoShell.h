             /*-----------------------------------*/
             /*          >>>PicoShell<<<          */
             /*            Theo D'Hondt           */
             /*   VUB Programming Technology Lab  */
             /*             (c) 2002              */
             /*-----------------------------------*/
             /*       PicoShell header file       */
             /*-----------------------------------*/
             /* Adapted somewhat by Dirk van Deun */
             /*         for wxPico (2004)         */
             /*-----------------------------------*/


/*------------------------- public types --------------------------*/

typedef unsigned _SHELL_;

/* Theo wrote:
 *    typedef struct _SHELL_ * _SHELL_;
 * This is an abomination, and g++ gets very very confused -- Dirk */

/*--------------------- callback prototypes -----------------------*/

/*-----------------------------------------------------------------*/
/*  _SHELL_ERROR_:                                                 */
/*     called when error is encountered                            */
/*                                                                 */
/*  arguments:                                                     */
/*     char *         ->     error text                            */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_ERROR_(const char *);
void _SHELL_ERROR_IN_WINDOW_(const char *, const void *);   /* dvd */

/*-----------------------------------------------------------------*/
/*  _SHELL_TEXT_LENGTH_:                                           */
/*     return length of window text                                */
/*                                                                 */
/*  arguments:                                                     */
/*     char *         ->     window                                */
/*                                                                 */
/*  returns:                                                       */
/*     unsigned       ->     length of text                        */
/*-----------------------------------------------------------------*/

unsigned _SHELL_TEXT_LENGTH_(const void *);

/*-----------------------------------------------------------------*/
/*  _SHELL_SET_SELECTION_:                                         */
/*     highlight selection                                         */
/*                                                                 */
/*  arguments:                                                     */
/*     void *         ->     window                                */
/*     unsigned       ->     start offset of selection             */
/*     unsigned       ->     end offset of selection               */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_SET_SELECTION_(const void *  ,
                           const unsigned, 
                           const unsigned);
void _SHELL_MARK_SELECTION_(const void *  ,
                           const unsigned,
                           const unsigned);
/* need to make the difference explicit to fix a problem in wxPico */
/* the new form is to mark errors, the original for anything else  */
                                    
/*-----------------------------------------------------------------*/
/*  _SHELL_PRINT_STRING_:                                          */
/*     print string in window                                      */
/*                                                                 */
/*  arguments:                                                     */
/*     void *         ->     window                                */
/*     char *         ->     string value                          */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_PRINT_STRING_(const void *,
                          const char *);

/*-----------------------------------------------------------------*/
/*  _SHELL_DISPLAY_STRING_:                                        */
/*     display string in window                                    */
/*                                                                 */
/*  arguments:                                                     */
/*     void *         ->     window                                */
/*     char *         ->     string value                          */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_DISPLAY_STRING_(const void *,
                            const char *);

/*-----------------------------------------------------------------*/
/*  _SHELL_GET_SELECTION_:                                         */
/*     get position of current selection                           */
/*                                                                 */
/*  arguments:                                                     */
/*     void *         ->     window                                */
/*     unsigned *     ->     address of start offset of selection  */
/*     unsigned *     ->     address of end offset of selection    */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_GET_SELECTION_(const void *      ,
                           unsigned *  , 
                           unsigned *  );

/*-----------------------------------------------------------------*/
/*  _SHELL_COPY_SELECTION_:                                        */
/*     copy current selection to string buffer                     */
/*                                                                 */
/*  arguments:                                                     */
/*     void *         ->     window                                */
/*     unsigned       ->     start offset of selection             */
/*     unsigned       ->     end offset of selection               */
/*     char *         ->     string buffer                         */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_COPY_SELECTION_(const void *  ,
                            const unsigned, 
                            const unsigned,
                                  char *  );

/*-----------------------------------------------------------------*/
/*  _SHELL_LOAD_FILE_:                                             */
/*     load file contents to string buffer                         */
/*                                                                 */
/*  arguments:                                                     */
/*     char *         ->     name of file                          */
/*     char *         ->     string buffer                         */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_LOAD_FILE_(const char *,
                             char *, void *);  /* dvd added window */


/*-----------------------------------------------------------------*/
/*  _SHELL_ACCEPT_PROMPT_:                                         */
/*     generate audio prompt for user input                        */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_ACCEPT_PROMPT_(const void *);

/*-----------------------------------------------------------------*/
/*  _SHELL_RECLAIM_ALERT_:                                         */
/*     generate audio signal for garbage collection                */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_RECLAIM_ALERT_(void);

/*-----------------------------------------------------------------*/
/*  _SHELL_BUSY_ALERT_:                                            */
/*     generate audio signal for busy evaluator                    */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_BUSY_ALERT_(void);

/*-----------------------------------------------------------------*/
/*  _SHELL_EVENTS_:                                                */
/*     let front-end process events                                */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_EVENTS_(void);

/*--------------------- exported prototypes -----------------------*/

/*-----------------------------------------------------------------*/
/*  _SHELL_VERSION_:                                               */
/*     Pico version string                                         */
/*                                                                 */
/*  arguments:                                                     */
/*     char *           ->     string to hold version              */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_VERSION_(char *);

/*-----------------------------------------------------------------*/
/*  _SHELL_START_:                                                 */
/*     set up Pico shell manager                                   */
/*                                                                 */
/*  arguments:                                                     */
/*     void *           ->     memory address                      */
/*     unsigned         ->     memory size                         */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_START_(const void *  , 
                   const unsigned);

/*-----------------------------------------------------------------*/
/*  _SHELL_STOP_:                                                  */
/*     remove Pico shell manager                                   */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_STOP_(void);

/*-----------------------------------------------------------------*/
/*  _SHELL_RESET_:                                                 */
/*     reset Pico shell manager                                    */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_RESET_(void);

/*-----------------------------------------------------------------*/
/*  _SHELL_MAKE_:                                                  */
/*     reset Pico                                                  */
/*                                                                 */
/*  arguments:                                                     */
/*     void *           ->     window                              */
/*                                                                 */
/*  returns: none                                                  */
/*     _SHELL_          ->     shell                               */
/*-----------------------------------------------------------------*/

_SHELL_ _SHELL_MAKE_(const void *);

/*-----------------------------------------------------------------*/
/*  _SHELL_FREE_:                                                  */
/*     remove Pico shell                                           */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_FREE_(const _SHELL_);

/*-----------------------------------------------------------------*/
/*  _SHELL_EVAL_:                                                  */
/*     evaluate Pico text                                          */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_EVAL_(const _SHELL_);

/*-----------------------------------------------------------------*/
/*  _SHELL_PROCEED_:                                               */
/*     proceed with Pico processing                                */
/*                                                                 */
/*  arguments: none                                                */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_PROCEED_(void);

/*-----------------------------------------------------------------*/
/*  _SHELL_ENTER_:                                                 */
/*     process enter command                                       */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_ENTER_(const _SHELL_);

/*-----------------------------------------------------------------*/
/*  _SHELL_LOAD_:                                                  */
/*     evaluate Pico textfile                                      */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_LOAD_(const _SHELL_, 
                  const char * );

/*-----------------------------------------------------------------*/
/*  _SHELL_INTERRUPT_:                                             */
/*     interrupt evaluation                                        */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_INTERRUPT_(const _SHELL_);

/*-----------------------------------------------------------------*/
/*  _SHELL_BUSY_:                                                  */
/*     check if evaluating                                         */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns:                                                       */
/*     short          ->     busy/not busy                         */
/*-----------------------------------------------------------------*/

short _SHELL_BUSY_(const _SHELL_);
short _SHELL_ENTERING_(const _SHELL_);  /* added for wxPico */
short _SHELL_COUNT_();  /* added for wxPico */

/*-----------------------------------------------------------------*/
/*  _SHELL_BALANCE_:                                               */
/*     balance character pairs                                     */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*                                                                 */
/*  returns: none                                                  */
/*-----------------------------------------------------------------*/

void _SHELL_BALANCE_(const _SHELL_);

/*-----------------------------------------------------------------*/
/*  _SHELL_FIND_:                                                  */
/*     find character string                                       */
/*                                                                 */
/*  arguments:                                                     */
/*     _SHELL_        ->     shell                                 */
/*     unsigned       ->     starting offset                       */
/*     char *         ->     search string                         */
/*                                                                 */
/*  returns:                                                       */
/*     short          ->     found/not found                       */
/*-----------------------------------------------------------------*/

short _SHELL_FIND_(const _SHELL_ ,
                   const unsigned,
                   const char *  );

