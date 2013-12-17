#include "View.h"
#include "Editor.h"

IMPLEMENT_DYNAMIC_CLASS(View, wxView) // docview magic

void View::OnDraw (wxDC *dc) {
}

bool View::OnCreate(wxDocument *doc, long WXUNUSED(flags)) {
	editor = new Editor(doc,this);

	editor->Show(true);
	Activate(true);

	return true;
}

bool View::OnClose(bool deleteWindow) {
	if (!GetDocument()->Close())
		return false;
	Activate(false);
	if (deleteWindow) {
		delete editor;
	}
	return true;
}
