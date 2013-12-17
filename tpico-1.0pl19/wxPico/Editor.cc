#include "Editor.h"
#include "Transcript.h"
#include "View.h"
#include "Menu.h"
#include "Application.h"
#include "DocManager.h"

extern "C" {
#include "../PicoShell/PicoShell.h"
}

Editor::Editor(wxDocument *d, View *v): 
wxDocChildFrame(d, v, GLOBAL_TRANSCRIPT, -1, "Editor", wxDefaultPosition,
                CHILD_SIZE), view(v), doc(d), preserving(true), timer(this) {

	wxMenu *file_menu = new wxMenu;
	wxMenu *edit_menu = new wxMenu;
	wxMenu *view_menu = new wxMenu;
	wxMenu *run_menu = new wxMenu;
	wxMenu *pico_menu = new wxMenu;
	wxMenu *help_menu = new wxMenu;

	file_menu->Append(wxID_NEW, "&New");
	file_menu->Append(wxID_OPEN, "&Open...");
	file_menu->Append(wxID_SAVE, "&Save");
	file_menu->Append(wxID_SAVEAS, "Save &As...");
	file_menu->Append(wxID_CLOSE, "&Close");
	file_menu->AppendSeparator();
	file_menu->Append(wxID_EXIT, "E&xit");
	GLOBAL_DOCMANAGER->FileHistoryUseMenu(file_menu);
	edit_menu->Append(MENU_EDIT_UNDO, "&Undo");
        edit_menu->AppendSeparator();
	edit_menu->Append(MENU_EDIT_COPY, "&Copy\tCtrl+C");
	edit_menu->Append(MENU_EDIT_CUT, "Cu&t\tCtrl+X");
	edit_menu->Append(MENU_EDIT_PASTE, "&Paste\tCtrl+V");
	view_menu->Append(MENU_VIEW_FONT, "&Font");
	run_menu->Append(MENU_RUN_SELECTION, "&Selection\tCtrl+S");
#ifdef __WXMAC__
	run_menu->Append(MENU_RUN_LINE, "&Line\tCtrl+L");
	run_menu->AppendSeparator();
	run_menu->Append(MENU_RUN_BREAK, "&Break\tCtrl+B");
	// wxmac translates ctrl-enter to little-flower-like-thing-enter...
	// ctrl-enter and ctrl-space do still work however
#else
	run_menu->Append(MENU_RUN_LINE, "&Line\tCtrl+Enter");
	run_menu->AppendSeparator();
	run_menu->Append(MENU_RUN_BREAK, "&Break\tCtrl+Space");
#endif
        pico_menu->Append(MENU_PICO_RESET,"Reset");
	help_menu->Append(MENU_HELP_ABOUT, "&About");
	menubar = new wxMenuBar;
	menubar->Append(file_menu, "&File");
	menubar->Append(edit_menu, "&Edit");
	menubar->Append(view_menu, "&View");
	menubar->Append(run_menu, "&Run");
	menubar->Append(pico_menu, "&Pico");
	menubar->Append(help_menu, "&Help");
	SetMenuBar(menubar);
	
        menubar->Enable(menubar->FindMenuItem(wxString("&Edit"),
				wxString("&Undo")),false);
        SetRunning(false);

	text = new wxTextCtrl(this, -1, "", wxDefaultPosition, wxDefaultSize,
                              wxTE_MULTILINE|wxTE_PROCESS_ENTER|
			      wxTE_RICH|wxHSCROLL); // for MS Windows
        text->SetFocus(); 
	SetFocus();
	Show(true);

	shell = _SHELL_MAKE_(this);

        wxTextAttr attr;
        wxFont font = text->GetDefaultStyle().GetFont();
        font.SetFamily(wxMODERN); // guaranteed to be a fixed pitch font
#ifdef __WXGTK__
        font.SetDefaultEncoding(wxFONTENCODING_ISO8859_1);
#endif
	attr.SetFont(font);
	text->SetDefaultStyle(attr);
#ifdef __WXGTK__
	preserving = false;
	text->AppendText(" ");
	text->SetStyle(0, text->GetValue().Len(), attr);
	text->Clear();
	preserving = true;
	text->SetFont(font);
#endif

	// strange and badly documented stuff...
	wxAcceleratorEntry accEntry[2];
	wxMenuItem* ret = new wxMenuItem(NULL,MENU_RUN_CTRL_ENTER,"&Accept");
	wxMenuItem* space = new wxMenuItem(NULL,MENU_RUN_BREAK,"&Break");
	accEntry[0].Set((int)wxACCEL_CTRL, (int) WXK_RETURN, 
			MENU_RUN_CTRL_ENTER, ret);
	accEntry[1].Set((int)wxACCEL_CTRL, (int) WXK_SPACE, 
			MENU_RUN_BREAK, space);
	wxAcceleratorTable accTable(2,accEntry);
	SetAcceleratorTable(accTable);

	savedBox[0]=0;
	timer.Start(100);
}

Editor::~Editor() {
	_SHELL_FREE_(shell);
	timer.Stop();
}

BEGIN_EVENT_TABLE(Editor, wxDocChildFrame)
	EVT_MENU(MENU_EDIT_UNDO, Editor::OnUndo)
	EVT_MENU(MENU_EDIT_COPY, Editor::OnCopy)
	EVT_MENU(MENU_EDIT_CUT, Editor::OnCut)
	EVT_MENU(MENU_EDIT_PASTE, Editor::OnPaste)
	EVT_MENU(MENU_VIEW_FONT, Editor::OnFont)
        EVT_MENU(MENU_RUN_SELECTION, Editor::OnRunSelection)
        EVT_MENU(MENU_RUN_LINE, Editor::OnRunLine)
        EVT_MENU(MENU_RUN_BREAK, Editor::OnBreak)
        EVT_MENU(MENU_RUN_ENTER, Editor::OnEnter)
        EVT_MENU(MENU_RUN_CTRL_ENTER, Editor::OnCtrlEnter)
	EVT_TEXT_ENTER(-1, Editor::OnEnter)
        EVT_TEXT(-1,Editor::OnChange)
	EVT_TIMER(-1,Editor::OnTimer)
	//EVT_CLOSE(Editor::OnCloseWindow)
END_EVENT_TABLE()

void Editor::OnUndo(wxCommandEvent& WXUNUSED(event)) {
        preserving = false;
	
	currenttext = prevtext;
	currentpos = prevpos;
	prevtext = text->GetValue();
	prevpos = text->GetInsertionPoint();
	text->SetValue(currenttext);
	text->SetInsertionPoint(currentpos);

        preserving = true;
        doc->Modify(true);
}

void Editor::preserve() {
	if (preserving && !_SHELL_ENTERING_(shell)) {
		menubar->Enable(menubar->FindMenuItem(wxString("&Edit"),
					wxString("&Undo")),true);
		prevtext = currenttext;
		currenttext = text->GetValue();
		prevpos = currentpos;
		currentpos = text->GetInsertionPoint();
	}
}

void Editor::OnChange(wxCommandEvent& WXUNUSED(event)) {
	preserve();
}

void Editor::OnCopy(wxCommandEvent& WXUNUSED(event)) {
	text->Copy();
}

void Editor::OnCut(wxCommandEvent& WXUNUSED(event)) {
	text->Cut();
}

void Editor::OnPaste(wxCommandEvent& WXUNUSED(event)) {
	text->Paste();
}

void Editor::OnFont(wxCommandEvent& WXUNUSED(event)) { 
	wxFontData data;
	preserving = false;
	data.SetInitialFont(text->GetDefaultStyle().GetFont());
	wxFontDialog dialog(text, data);
	if (dialog.ShowModal() == wxID_OK) {
		text->SetFont(dialog.GetFontData().GetChosenFont());
		//text->SetTextColour(dialog.GetFontData().GetColour());
		wxTextAttr attr;
		attr.SetFont(dialog.GetFontData().GetChosenFont());
		attr.SetTextColour(dialog.GetFontData().GetColour());
		text->SetDefaultStyle(attr);
		text->SetStyle(0, text->GetValue().Len(), attr);
	}
	preserving = true;
}

void Editor::SetRunning(bool val) {
        menubar->Enable(menubar->FindMenuItem(wxString("&Run"),
                 wxString("&Selection")),!val);
        menubar->Enable(menubar->FindMenuItem(wxString("&Run"),
                 wxString("&Line")),!val);
        menubar->Enable(menubar->FindMenuItem(wxString("&Run"),
                 wxString("&Break")),val);
}

void Editor::OnRunSelection(wxCommandEvent& WXUNUSED(event)) {
	preserve();
        SetRunning(true);
        loading = false;
	_SHELL_EVAL_(shell);
}

void Editor::OnRunLine(wxCommandEvent& WXUNUSED(event)) {
        long point, column, line;
        SetRunning(true);
	preserve();
        point = text->GetInsertionPoint();
#ifdef __WXMAC__
	// PositionToXY not implemented on wxmac...
	wxString tmp = text->GetValue();
	unsigned start = point-1, stop = point;
	while (start && tmp[start] != '\r') start--;
	while (tmp[stop] != '\r') stop++;
	text->SetSelection(start,stop);
#else
        text->PositionToXY(point, &column, &line);
        text->SetSelection(point-column, point-column+
              text->GetLineLength(line));
#endif
        loading = false;
        _SHELL_EVAL_(shell);
}

void Editor::OnBreak(wxCommandEvent& WXUNUSED(event)) {
        SetRunning(false);
	_SHELL_INTERRUPT_(shell);
}

void Editor::OnEnter(wxCommandEvent& WXUNUSED(event)) { 
        if (_SHELL_ENTERING_(shell))
                _SHELL_ENTER_(shell);
        else {
                long point, column, line;
                point = text->GetInsertionPoint();
                text->PositionToXY(point, &column, &line);
                wxString prevline = text->GetLineText(line);
                int i = 0, length = prevline.Len();
                text->WriteText("\n");
                while (i<length) {
                        char c = prevline.GetChar(i++);
                        if (c == ' ' || c == '\t')
                                text->WriteText(c);
                        else
                                break;
                } // minimal automatic indentation
		preserve();
        }
}

void Editor::OnCtrlEnter(wxCommandEvent& event) { 
        if (_SHELL_ENTERING_(shell))
                _SHELL_ENTER_(shell);
        else
		OnRunLine(event);
}

void Editor::OnTimer(wxTimerEvent& WXUNUSED(event)) {
#ifdef __WXGTK__
	// pops up windows for the pico thread, which cannot do so reliably
	// no worries, there is no next timer event until this one is finished
	if (savedBox[0]) {
		(void)wxMessageBox(savedBox, "Error", wxICON_ERROR);
		savedBox[0]=0;
	}
#endif
}

void Editor::OnCloseWindow(wxCloseEvent& event) {
	wxDocChildFrame::OnCloseWindow(event);
}

