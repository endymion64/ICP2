#ifndef EDITOR_H
#define EDITOR_H

#include <wx/wx.h>
#include <wx/docview.h>
#include <wx/fontdlg.h>

// taken from PicoShell.c
#define BUFFER_SIZE            0xFFFF

struct View;

struct Editor: public wxDocChildFrame {

	wxTextCtrl *text;
        wxMenuBar *menubar;
        View *view;
        wxDocument *doc;

	unsigned shell;
        bool loading;

        Editor(wxDocument *d, View *v);
        ~Editor();

	void preserve();

        bool preserving;
        wxString prevtext, currenttext;
	long prevpos, currentpos;

	void SetRunning(bool val);

	char savedBox[BUFFER_SIZE];
	wxTimer timer;

DECLARE_EVENT_TABLE()

	void OnUndo(wxCommandEvent& WXUNUSED(event));
	void OnCopy(wxCommandEvent& WXUNUSED(event));
	void OnCut(wxCommandEvent& WXUNUSED(event));
	void OnPaste(wxCommandEvent& WXUNUSED(event));
	void OnFont(wxCommandEvent& WXUNUSED(event));
	void OnRunLine(wxCommandEvent& WXUNUSED(event));
	void OnRunSelection(wxCommandEvent& WXUNUSED(event));
        void OnBreak(wxCommandEvent& WXUNUSED(event));
        void OnEnter(wxCommandEvent& WXUNUSED(event));
        void OnCtrlEnter(wxCommandEvent& event);
        void OnChange(wxCommandEvent& WXUNUSED(event));
	void OnTimer(wxTimerEvent& WXUNUSED(event));
	void OnCloseWindow(wxCloseEvent& event);

};

#endif
