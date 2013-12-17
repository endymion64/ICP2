#include "Application.h"
#include "Transcript.h"
#include "Document.h"
#include "View.h"
#include "DocManager.h"
#include "Editor.h"

extern "C" {
#include "../Pico/Pico.h"
#include "../Pico/PicoMai.h"
#include "../PicoShell/PicoShell.h"
}

#define MEMSIZE                10*1024*1024

// taken from PicoShell.c
#define BUFFER_OVERFLOW        "Buffer overflow"
#define CANNOT_OPEN_LOAD_FILE  "Cannot open load file"
#define BUFFER_SIZE            0xFFFF

void *GLOBAL_PICOMEMORY;
volatile bool GLOBAL_QUIT;

static void GuiEnter() {
#ifdef __WXGTK__
	wxMutexGuiEnter();
#endif
}

static void GuiLeave() {
#ifdef __WXGTK__
	wxMutexGuiLeave();
#endif
}

extern "C" {

void _SHELL_ERROR_(const char *text) {
	// no GuiMutex: _SHELL_ERROR_ is not called from the interpreter loop
	(void)wxMessageBox(text, "Error", wxICON_ERROR);
}

void _SHELL_ERROR_IN_WINDOW_(const char *text, const void *window) {
	GuiEnter();
	// opening a new window in another thread than the primary one is not
	// supported on wxGTK/OpenBSD, so we save the message and let the
	// foreground process check regularly
#ifdef __WXGTK__
	strcpy(((Editor *)window)->savedBox,text);
#else
	(void)wxMessageBox(text, "Error", wxICON_ERROR);
#endif
        ((Editor *)window)->SetRunning(false);
	GuiLeave();
}

unsigned _SHELL_TEXT_LENGTH_(const void *window) {
	unsigned len;
	GuiEnter();
	len = ((Editor *)window)->text->GetLastPosition();
	GuiLeave();
	return len;
}

void _SHELL_SET_SELECTION_(const void *window, unsigned start, unsigned end) {
	GuiEnter();
	((Editor *)window)->text->SetSelection(start, end);
	GuiLeave();
}

void _SHELL_MARK_SELECTION_(const void *window, unsigned start, unsigned end) {
	GuiEnter();
        if (!((Editor *)window)->loading)
		((Editor *)window)->text->SetSelection(start, end);
	GuiLeave();
}

void _SHELL_PRINT_STRING_(const void *window, const char *text) {
	long longstart, longend;
	GuiEnter();
	((Editor *)window)->text->SetSelection(0, 0);
	((Editor *)window)->text->SetInsertionPointEnd();
	((Editor *)window)->text->AppendText(text);
        ((Editor *)window)->SetRunning(false);
	GuiLeave();
}

void _SHELL_DISPLAY_STRING_(const void *window, const char *text) {
	long longstart, longend;
	GuiEnter();
	((Editor *)window)->text->SetSelection(0, 0);
	((Editor *)window)->text->SetInsertionPointEnd();
	((Editor *)window)->text->AppendText(text);
	GuiLeave();
}

void _SHELL_GET_SELECTION_(const void *window, unsigned *start, unsigned *end) {
	long longstart, longend;
	GuiEnter();
	((Editor *)window)->text->GetSelection(&longstart, &longend);
	*start = (unsigned)longstart ; *end = (unsigned)longend;
	GuiLeave();
}

void _SHELL_COPY_SELECTION_(const void *window, const unsigned start,
		const unsigned end, char *buffer) {
	GuiEnter();
	strcpy(buffer, ((Editor *)window)->text->GetRange(start, end).c_str());
	GuiLeave();
}

void _SHELL_LOAD_FILE_(const char *filename, char *buffer, void *window) {
	GuiEnter(); // if only to protect the SHELL_ERROR's
	if (!wxFileExists(filename)) {
		_SHELL_ERROR_(CANNOT_OPEN_LOAD_FILE);
		strcpy(buffer,"display("")\0"); // so pico proceeds correctly
		return;
	}
        ((Editor *)window)->loading = true;
	wxFile file(filename);
	if (file.Length() < BUFFER_SIZE) {
		file.Read(buffer, file.Length());
                buffer[file.Length()]='\0';
        }
	else
		_SHELL_ERROR_(BUFFER_OVERFLOW);
	GuiLeave();
}

void _SHELL_ACCEPT_PROMPT_(const void *window) {
	GuiEnter();
	((Editor *)window)->text->SetSelection(0, 0);
	((Editor *)window)->text->SetInsertionPointEnd();
	GuiLeave();
}

void _SHELL_RECLAIM_ALERT_(void) {
}

void _SHELL_BUSY_ALERT_(void) {
}

void _SHELL_EVENTS_(void) {
#ifndef __WXGTK__
	wxYield();
#endif
}
	
}

IMPLEMENT_APP(Application)

/* Updates of text windows during wxYield have been sabotaged in wxGTK (only
 * in GTK2) so I had to reintroduce threads instead of yielding.  The word
 * aaaargh does not even begin to express my feelings when I discovered this
 * interesting tidbit in the wxGTK source code. */

struct PicoThread: public wxThread {

	virtual ExitCode Entry() {
		while (!GLOBAL_QUIT) {
			_SHELL_PROCEED_(); 
			if (!_SHELL_COUNT_())
				wxThread::Sleep(10); // milliseconds
		} // not very elegant, but it works fine
		wxGetApp().ExitMainLoop();
	}
};

bool Application::OnInit(void) {

	GLOBAL_DOCMANAGER = new DocManager;
	GLOBAL_TRANSCRIPT = new Transcript(GLOBAL_DOCMANAGER);

	GLOBAL_TEMPLATE = new wxDocTemplate(GLOBAL_DOCMANAGER, "Pico Files",
			"*.pco", "", "pco", "Pico document", "Pico view",
			CLASSINFO(Document), CLASSINFO(View));
        GLOBAL_DOCMANAGER->SetMaxDocsOpen(_MAX_SES_);	
	GLOBAL_TRANSCRIPT->Show(true); // GDK-errors if hidden

	assert((GLOBAL_PICOMEMORY = malloc(MEMSIZE)) != NULL);
	_SHELL_START_(GLOBAL_PICOMEMORY, MEMSIZE);

        GLOBAL_DOCMANAGER->CreateDocument("", wxDOC_NEW);

	GLOBAL_QUIT = false;

#ifdef __WXGTK__
	PicoThread *t = new PicoThread();
	t->Create();
	t->Run();
	return true;
#else
	while (!GLOBAL_QUIT) {
		_SHELL_PROCEED_(); 
                if (!_SHELL_COUNT_())
                        wxThread::Sleep(10); // milliseconds
	} // not very elegant, but it works fine
	return false; // do not continue the wxWidgets main loop
#endif
}
