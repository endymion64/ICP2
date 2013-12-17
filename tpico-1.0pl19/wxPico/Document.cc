#include "Document.h"
#include "View.h"
#include "Editor.h"

IMPLEMENT_DYNAMIC_CLASS(Document, wxDocument) // docview magic

bool
Document::OnSaveDocument(const wxString& filename) {
	View *v = (View*)GetFirstView();
	if (!v->editor->text->SaveFile(filename))
		return false;
	Modify(false);
        m_savedYet = true; // dirty hack, there's probably a better way
	return true;
}

bool
Document::OnOpenDocument(const wxString& filename) {
	View *v = (View*)GetFirstView();
	if (!v->editor->text->LoadFile(filename))
		return false;
	SetFilename(filename, true);
	Modify(false);
	UpdateAllViews();
        m_savedYet = true; // dirty hack again
	return true;
}

bool
Document::IsModified(void) const {
	View *v = (View*)GetFirstView();
	if (v)
		return (wxDocument::IsModified() || 
                        v->editor->text->IsModified());
	else
		return wxDocument::IsModified();
}

void Document::Modify(bool mod) {
	View *v = (View*)GetFirstView();
	wxDocument::Modify(mod);
	if (!mod && v && v->editor->text)
		v->editor->text->DiscardEdits();
}
