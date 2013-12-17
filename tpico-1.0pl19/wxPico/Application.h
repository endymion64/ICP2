#ifndef APPLICATION_H
#define APPLICATION_H

#include <wx/wx.h>
#include <wx/docview.h>
#include <wx/file.h>

struct Application: public wxApp {

	bool OnInit(void);

};

DECLARE_APP(Application)

extern volatile bool GLOBAL_QUIT;

#endif
