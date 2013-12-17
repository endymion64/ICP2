#ifndef DOCUMENT_H
#define DOCUMENT_H

#include <wx/wx.h>
#include <wx/docview.h>

struct Document: public wxDocument {

DECLARE_DYNAMIC_CLASS(Document) // necessary for CLASSINFO in Application.cc

	virtual bool OnSaveDocument(const wxString& filename);
	virtual bool OnOpenDocument(const wxString& filename);
	virtual bool IsModified(void) const;
	virtual void Modify(bool mod);

};

#endif
