#ifndef TRANSCRIPT_H
#define TRANSCRIPT_H

#include <wx/wx.h>
#include <wx/docview.h>

struct DocManager; 

struct Transcript: public wxDocParentFrame {

	wxMenuBar *menubar;

	Transcript(DocManager *m);

DECLARE_EVENT_TABLE()
	 
	void OnReset(wxCommandEvent& WXUNUSED(event));
	void OnAbout(wxCommandEvent& WXUNUSED(event));
	void OnCloseWindow(wxCloseEvent& event);

};

extern Transcript *GLOBAL_TRANSCRIPT;
extern wxSize CHILD_SIZE;

#endif
