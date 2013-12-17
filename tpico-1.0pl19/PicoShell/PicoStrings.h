             /*-----------------------------------*/
             /*          >>>PicoShell<<<          */
             /*            Theo D'Hondt           */
             /*   VUB Programming Technology Lab  */
             /*             (c) 2002              */
             /*-----------------------------------*/
             /*    PicoStrings 1.0 header file    */
             /*-----------------------------------*/
             
#define PICO_VERSION   "      Pico 1.0 (August 1997)      "

#define AGR_ERROR_TEXT "abstract grammar violation"
#define ATC_ERROR_TEXT "argument type conflict"
#define BUF_ERROR_TEXT "text buffer overflow"
#define CTL_ERROR_TEXT "control violation"
#define DCT_ERROR_TEXT "too many names"
#define DIG_ERROR_TEXT "digit required"
#define DPS_ERROR_TEXT "duplicate session identifier"
#define EXP_ERROR_TEXT "expression violation"
#define EXT_ERROR_TEXT "excess token(s)"
#define IAG_ERROR_TEXT "invalid argument"
#define IDX_ERROR_TEXT "table index violation"
#define IIX_ERROR_TEXT "invalid index"
#define ILL_ERROR_TEXT "illegal character"
#define INF_ERROR_TEXT "infinite value"
#define IPM_ERROR_TEXT "invalid parameter"
#define MEM_ERROR_TEXT "storage overflow"
#define NAB_ERROR_TEXT "not a boolean"
#define NAF_ERROR_TEXT "not a function"
#define NAT_ERROR_TEXT "not a table"
#define NBR_ERROR_TEXT "number too large"
#define NEG_ERROR_TEXT "negative argument"
#define NMA_ERROR_TEXT "non-matching argument list"
#define RBC_ERROR_TEXT "right brace expected"
#define RBR_ERROR_TEXT "right bracket expected"
#define REE_ERROR_TEXT "reentrancy violation"
#define REF_ERROR_TEXT "reference expected"
#define RNG_ERROR_TEXT "range error"
#define RPR_ERROR_TEXT "right parenthesis expected"
#define SIZ_ERROR_TEXT "invalid size"
#define SNA_ERROR_TEXT "session not active"
#define STK_ERROR_TEXT "stack error"
#define TAG_ERROR_TEXT "invalid tag"
#define TMS_ERROR_TEXT "too many sessions"
#define TXT_ERROR_TEXT "non-terminated text"
#define UDI_ERROR_TEXT "undefined identifier"
#define USR_ERROR_TEXT "user error"
#define ZDV_ERROR_TEXT "zero division"

static char * PicoErrorMessages[] = 
  { AGR_ERROR_TEXT,
		ATC_ERROR_TEXT,
		BUF_ERROR_TEXT,
		CTL_ERROR_TEXT,
		DCT_ERROR_TEXT,
		DIG_ERROR_TEXT,
		DPS_ERROR_TEXT,
		EXP_ERROR_TEXT,
		EXT_ERROR_TEXT,
		IAG_ERROR_TEXT,
		IDX_ERROR_TEXT,
		IIX_ERROR_TEXT,
		ILL_ERROR_TEXT,
		INF_ERROR_TEXT,
		IPM_ERROR_TEXT,
		MEM_ERROR_TEXT,
		NAB_ERROR_TEXT,
		NAF_ERROR_TEXT,
		NAT_ERROR_TEXT,
		NBR_ERROR_TEXT,
		NEG_ERROR_TEXT,
		NMA_ERROR_TEXT,
		RBC_ERROR_TEXT,
		RBR_ERROR_TEXT,
		REE_ERROR_TEXT,
		REF_ERROR_TEXT,
		RNG_ERROR_TEXT,
		RPR_ERROR_TEXT,
		SIZ_ERROR_TEXT,
		SNA_ERROR_TEXT,
		STK_ERROR_TEXT,
		TAG_ERROR_TEXT,
		TMS_ERROR_TEXT,
		TXT_ERROR_TEXT,
		UDI_ERROR_TEXT,
		USR_ERROR_TEXT,
		ZDV_ERROR_TEXT };

		
#define PICO_ERROR(ERR)\
  PicoErrorMessages[ERR - 1]	
		
		
