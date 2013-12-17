#include "Transcript.h"
#include "Application.h"
#include "Menu.h"
#include "DocManager.h"

extern "C" {
#include "../PicoShell/PicoShell.h"
}

#define MIN(x,y) ((x)>(y)?(y):(x))
#define MAX(x,y) ((x)<(y)?(y):(x))

Transcript *GLOBAL_TRANSCRIPT;
wxSize CHILD_SIZE;

Transcript::Transcript(DocManager *m): wxDocParentFrame (m, NULL, -1,
		"wxPico 1.0" /*, wxPoint(0,0) */) {

	wxMenu *file_menu = new wxMenu;
	wxMenu *pico_menu = new wxMenu;
	wxMenu *help_menu = new wxMenu;

	file_menu->Append(wxID_NEW, "&New");
	file_menu->Append(wxID_OPEN, "&Open...");
	file_menu->AppendSeparator();
	file_menu->Append(wxID_EXIT, "E&xit");
        pico_menu->Append(MENU_PICO_RESET,"Reset");
	help_menu->Append(MENU_HELP_ABOUT, "&About");

	GLOBAL_DOCMANAGER->FileHistoryUseMenu(file_menu);

	menubar = new wxMenuBar;
	menubar->Append(file_menu, "&File");
	menubar->Append(pico_menu, "&Pico");
	menubar->Append(help_menu, "&Help");

	SetMenuBar(menubar);

        int width, height;
        GetSize(&width, &height);
	width = MAX(width,wxSystemSettings::GetMetric(wxSYS_SCREEN_X)*2/3);
	height = MAX(height,wxSystemSettings::GetMetric(wxSYS_SCREEN_Y)*2/3);
        CHILD_SIZE = wxSize(width,height);

#ifdef __WXMAC__
	Move(wxSystemSettings::GetMetric(wxSYS_SCREEN_X)+1,
	     wxSystemSettings::GetMetric(wxSYS_SCREEN_Y)+1);
	// out of sight

	wxGetApp().s_macAboutMenuItemId = MENU_HELP_ABOUT;
	//see http://wiki.wxwindows.org/wiki.pl?WxMac_Issues
#endif
}

BEGIN_EVENT_TABLE(Transcript, wxDocParentFrame)
        EVT_MENU(MENU_PICO_RESET, Transcript::OnReset)
	EVT_MENU(MENU_HELP_ABOUT, Transcript::OnAbout)
	EVT_CLOSE(Transcript::OnCloseWindow)
END_EVENT_TABLE()

void Transcript::OnCloseWindow(wxCloseEvent& event) {
	GLOBAL_QUIT = true;
	wxDocParentFrame::OnCloseWindow(event);
}

void Transcript::OnReset(wxCommandEvent& WXUNUSED(event)) {
        _SHELL_RESET_();
}

void Transcript::OnAbout(wxCommandEvent& WXUNUSED(event)) {
	(void)wxMessageBox("Pico Interpreter: Theo D'Hondt\n\
Graphical User Interface: Dirk van Deun",
                           "About wxPico", wxICON_INFORMATION);
}
