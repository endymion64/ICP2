CXX =      $(shell wx-config --cxx)
CXXFLAGS = $(shell wx-config --cxxflags)
LIBS =     $(shell wx-config --libs)

wxPico: config Application.o Transcript.o DocManager.o Document.o \
View.o Editor.o
	$(MAKE) -C ../Pico
	$(MAKE) -C ../PicoShell
	$(CXX) -o wxPico *.o ../Pico/*.o ../PicoShell/*.o $(LIBS) -lm 
	@if [ `uname` == Darwin ]; then \
	mkdir -p wxPico.app/Contents/MacOS wxPico.app/Contents/Resources; \
	cp Info.plist PkgInfo wxPico.app/Contents; \
	cp Pico.icns wxPico.app/Contents/Resources; \
	cp wxPico wxPico.app/Contents/MacOS; fi

install: wxPico
	install wxPico /usr/local/bin/wxPico-1
	ln -sf /usr/local/bin/wxPico-1 /usr/local/bin/wxPico

config:
	./configure

Application.o: Application.cc Application.h Transcript.h Document.h View.h \
DocManager.h Editor.h
	$(CXX) $(CXXFLAGS) -c Application.cc

Transcript.o: Transcript.cc Transcript.h Application.h Menu.h DocManager.h
	$(CXX) $(CXXFLAGS) -c Transcript.cc

DocManager.o: DocManager.cc DocManager.h
	$(CXX) $(CXXFLAGS) -c DocManager.cc
	
Document.o: Document.cc Document.h View.h Editor.h
	$(CXX) $(CXXFLAGS) -c Document.cc

View.o: View.cc View.h Editor.h
	$(CXX) $(CXXFLAGS) -c View.cc

Editor.o: Editor.cc Editor.h Transcript.h View.h Menu.h Application.h \
DocManager.h
	$(CXX) $(CXXFLAGS) -c Editor.cc

clean:
	rm -f wxPico wxPico.exe *.o *.exe core tags \
wxPico.app/Contents/MacOS/wxPico
