#include "DocManager.h"

DocManager *GLOBAL_DOCMANAGER;
wxDocTemplate *GLOBAL_TEMPLATE;

wxDocument * DocManager::CreateDocument(const wxString& path, long flags) {
        // standard behaviour is to silently close the oldest file if the
        // maximum is reached !
	if (GetDocuments().Number() >= m_maxDocsOpen)
		(void)wxMessageBox ("Maximum amount of sessions reached",
				    "Error", wxICON_ERROR);
	else return wxDocManager::CreateDocument(path, flags);
}
