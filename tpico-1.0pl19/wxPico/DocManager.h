#ifndef DOCMANAGER_H
#define DOCMANAGER_H

#include <wx/wx.h>
#include <wx/docview.h>

struct DocManager: public wxDocManager {

        wxDocument *CreateDocument(const wxString& path, long flags);
 
};

extern DocManager *GLOBAL_DOCMANAGER;
extern wxDocTemplate *GLOBAL_TEMPLATE;

#endif
