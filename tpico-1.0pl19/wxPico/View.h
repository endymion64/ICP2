#ifndef VIEW_H
#define VIEW_H

#include <wx/wx.h>
#include <wx/docview.h>

struct Editor;

struct View: public wxView {

DECLARE_DYNAMIC_CLASS(View) // necessary for CLASSINFO in Application.cc

	Editor *editor;

	void OnDraw(wxDC *dc); // necessary for correct compilation

	bool OnCreate(wxDocument *doc, long flags);
	bool OnClose(bool deleteWindow = true);

};

#endif
