#include <qapplication.h>
#include <qgl.h>
#include <qimage.h>

#include <GL/gl.h>

#include <miString.h>
#include <miTime.h>
#include <diFontManager.h>
#include <diColour.h>
#include <diPlotOptions.h>
#include <glMapPlot.h>
#include <glFieldPlot.h>

#include <vector>
#include <iostream>

#include <snapInterface.h>

using namespace std;


static int timer_interval = 100;  // timer interval (millisec)


class GLwidget : public QGLWidget
{
public:
    GLwidget( QWidget *parent=0, const char *name=0 );
private:
    MapPlot *map;
    FontManager* fp;
    float x1allmap,x2allmap,y1allmap,y2allmap;
    float x1map,x2map,y1map,y2map;
    float x1bar,x2bar,y1bar,y2bar;
    float x1tot,x2tot,y1tot,y2tot;
    int   ploth,plotw;
    int   xfirst,yfirst;
    bool  endOfRun;
    bool  zooming,panning;
    bool  useraction;
    bool  pause,onestep;
    bool  savePNG,saveXPM,saveBMP;
    miString timestring;
    float xact[2],yact[2];
    void pos2map(float &x, float &y);
    void draw();
protected:
    void initializeGL();
    void resizeGL( int width=0, int height=0 );
    void plot();
    void paintGL();
    void timerEvent( QTimerEvent * );
    void keyPressEvent(QKeyEvent *me);
    void mousePressEvent(QMouseEvent *me);
    void mouseMoveEvent(QMouseEvent *me);
    void mouseReleaseEvent(QMouseEvent *me);
};


GLwidget::GLwidget( QWidget *parent, const char *name )
     : QGLWidget( parent, name ),
       endOfRun(false), zooming(false), panning(false), useraction(false),
       pause(false), onestep(false), fp(0),
       savePNG(false), saveXPM(false), saveBMP(false), timestring("xxxx")
{
//#######################################################
//cerr << "GLwidget::GLwidget ... startTimer" << endl;
//#######################################################
    startTimer( timer_interval );

    // define colours

    Colour::define("white",        255,255,255,255);
    Colour::define("grayWhite",    224,224,224,255);
    Colour::define("lightGray",    192,192,192,255);
    Colour::define("gray",         160,160,164,255);
    Colour::define("darkGray",     128,128,128,255);
    Colour::define("black",        0,0,0,255);
    Colour::define("blue",         0,0,255,255);
    Colour::define("red",          255,0,0,255);
    Colour::define("green",        0,255,0,255);
    Colour::define("cyan",         0,255,255,255);
    Colour::define("magenta",      255,0,255,255);
    Colour::define("yellow",       255,255,0,255);
    Colour::define("lightBlue",    51,51,255,255);
    Colour::define("darkRed",      128,0,0,255);
    Colour::define("darkGreen",    0,128,0,255);
    Colour::define("darkBlue",     0,0,128,255);
    Colour::define("darkCyan",     0,128,128,255);
    Colour::define("darkMagenta",  128,0,128,255);
    Colour::define("darkYellow",   128,128,0,255);
    Colour::define("brown",        178,51,0,255);
    Colour::define("orange",       255,89,0,255);
    Colour::define("purple",       160,32,240,255);
    Colour::define("midnightBlue", 25,25,112,255);
    Colour::define("dnmiGreen",    43,120,36,255);
    Colour::define("dnmiBlue",     0,54,125,255);
    Colour::define("green2",       0,238,0,255);
    Colour::define("green3",       0,205,0,255);
    Colour::define("green4",       0,139,0,255);
    Colour::define("flesh",        240,158,92,255);
    Colour::define("seablue",      117,199,242,255);

}


void GLwidget::initializeGL()
{
//#######################################################
//cerr << "GLwidget::initializeGL" << endl;
//#######################################################

  setCaption("met.no SNAP");

  setMinimumSize( 700, 600 );

  //setMouseTracking(true);
  setMouseTracking(false);

//#######################################################################
  // get info and draw map (in GL list)
  int   gridtype;
  float gridspec[6];
  float gridpart[4];
  getmapspec_(&gridtype, gridspec, gridpart);

  vector<miString> landfiles;
  vector<float>    linewidths;
  vector<miString> colours;
  landfiles.clear();
  linewidths.clear();
  colours.clear();
  landfiles.push_back("/usr/local/share/diana/maps/land4.dat");
  landfiles.push_back("/usr/local/share/diana/maps/land5.dat");
//landfiles.push_back("/metno/local/maps/land6.dat");
  linewidths.push_back(2.);
  linewidths.push_back(1.);
//linewidths.push_back(1.);
  colours.push_back("darkGreen");
  colours.push_back("darkGreen");
//colours.push_back("darkGreen");

  map= new MapPlot(gridtype, gridspec, gridpart,
	           landfiles, colours, linewidths);

  x1allmap= x1map= gridpart[0] - 1.;
  x2allmap= x2map= gridpart[1] - 1.;
  y1allmap= y1map= gridpart[2] - 1.;
  y2allmap= y2map= gridpart[3] - 1.;
}


void GLwidget::resizeGL( int width, int height )
{
//#######################################################
//cerr << "GLwidget::resizeGL" << endl;
//cerr << "     width,height: " << width << " " << height << endl;
//#######################################################

    if (width>0 && height>0) {
      plotw= width;
      ploth= height;
    }

    float w,h,dybar,dxtot,dytot,dx,dy;

    w= plotw;
    h= ploth;
    dybar= (y2map-y1map)*0.03;
    dxtot= x2map-x1map;
    dytot= y2map-y1map + dybar;

    if (w/h > dxtot/dytot) {
      dx= dytot * w / h - dxtot;
      dy= 0.0;
    } else {
      dx= 0.0;
      dy= dxtot * h / w - dytot;
    }
//#######################################################
//cerr<<"    MAP INP: "<<x1map<<" "<<x2map<<" "<<y1map<<" "<<y2map<<endl;
//#######################################################
    x1map= x1map - dx*0.5;
    x2map= x2map + dx*0.5;
    y1map= y1map - dy*0.5;
    y2map= y2map + dy*0.5;

    x1bar= x1map;
    x2bar= x2map;
    y1bar= y2map + dybar*0.5;
    y2bar= y2map + dybar;
//#######################################################
//cerr<<"    MAP OUT: "<<x1map<<" "<<x2map<<" "<<y1map<<" "<<y2map<<endl;
//#######################################################

    x1tot= x1map;
    x2tot= x2map;
    y1tot= y1map;
    y2tot= y2bar;

    glViewport( 0, 0, plotw, ploth );
    //glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glOrtho(x1tot,x2tot,y1tot,y2tot,-1.,1.);

    // new map area
    map->resize(x1map,x2map,y1map,y2map);
}


void GLwidget::plot()
{
  updateGL();

  //..............................

  miString format, filename;
  int quality= -1; // default quality

  if (savePNG) {
    format="PNG";
    filename="snap_" + timestring + ".png";
  } else if (saveXPM) {
    format="XPM";
    filename="snap_" + timestring + ".xpm";
  } else if (saveBMP) {
    format="BMP";
    filename="snap_" + timestring + ".bmp";
  }

  if (filename.exists()) {
    makeCurrent();
    glFlush();

    QImage image= grabFrameBuffer(true); // withAlpha=TRUE
    image.save(filename.cStr(), format.cStr(), quality );
  }

}


void GLwidget::paintGL()
{
//#######################################################
//cerr << "GLwidget::paintGL" << endl;
//#######################################################
   draw();
}


void GLwidget::timerEvent(QTimerEvent*)
{
//#######################################################
//cerr << "GLwidget::timerEvent" << endl;
//#######################################################
  plot();

  if (endOfRun) {
    cerr << "END OF RUN........................" << endl;
    int iaction= 2;
    int iexit= 0;
    snap_(&iaction, &iexit);
    killTimers();
  }
}


void GLwidget::keyPressEvent(QKeyEvent *me)
{
//#######################################################
//cerr << "GLwidget::keyPressEvent" << endl;
//#######################################################
  if (me->key()==Key_Escape) qApp->quit();
  if (me->key()==Key_Q && me->state()==ControlButton) qApp->quit();

  if (me->key()==Key_Home) {
    x1map= x1allmap;
    x2map= x2allmap;
    y1map= y1allmap;
    y2map= y2allmap;
    resizeGL();
    plot();
  } else if (me->key()==Key_P) {
    pause= !pause;
  } else if (me->key()==Key_S) {
    onestep= true;
  }
}


//void GLwidget::keyReleaseEvent(QKeyEvent *me)
//{
//}


void GLwidget::mousePressEvent(QMouseEvent *me)
{
//#######################################################
//cerr << "GLwidget::mousePressEvent" << endl;
//#######################################################
  if (me->button()==LeftButton) {
    useraction= true;
    zooming= true;
    xfirst= me->x();
    yfirst= height() - me->y();
    xact[0]= xfirst;
    yact[0]= yfirst;
    pos2map(xact[0],yact[0]);
    xact[1]= xact[0];
    yact[1]= yact[0];
    setMouseTracking(true);
  } else if (me->button()==MidButton) {
    useraction= true;
    panning= true;
    xact[0]= me->x();
    yact[0]= height() - me->y();
    pos2map(xact[0],yact[0]);
    setMouseTracking(true);
  } else if (me->button()==RightButton) {
    float dx= (x2map-x1map)*0.2;
    float dy= (y2map-y1map)*0.2;
    x1map= x1map-dx;
    x2map= x2map+dx;
    y1map= y1map-dy;
    y2map= y2map+dy;
    resizeGL();
    plot();
  }
}


void GLwidget::mouseMoveEvent(QMouseEvent *me)
{
//#######################################################
//cerr << "GLwidget::mouseMoveEvent" << endl;
//#######################################################
  if (zooming) {
    xact[1]= me->x();
    yact[1]= height() - me->y();
    pos2map(xact[1],yact[1]);
    plot();
  } else if (panning) {
    xact[1]= me->x();
    yact[1]= height() - me->y();
    pos2map(xact[1],yact[1]);
    float dx= xact[0]-xact[1];
    float dy= yact[0]-yact[1];
    x1map= x1map+dx;
    x2map= x2map+dx;
    y1map= y1map+dy;
    y2map= y2map+dy;
    resizeGL();
    plot();
  }
}


void GLwidget::mouseReleaseEvent(QMouseEvent *me)
{
//#######################################################
//cerr << "GLwidget::mouseReleaseEvent" << endl;
//#######################################################
  if (zooming) {
    zooming= false;
    int xlast= me->x();
    int ylast= height() - me->y();
    if (abs(xlast-xfirst)>15 || abs(ylast-yfirst)>15) {
      xact[1]= xlast;
      yact[1]= ylast;
      pos2map(xact[1],yact[1]);
      x1map= (xact[0]<xact[1]) ? xact[0] : xact[1];
      x2map= (xact[0]<xact[1]) ? xact[1] : xact[0];
      y1map= (yact[0]<yact[1]) ? yact[0] : yact[1];
      y2map= (yact[0]<yact[1]) ? yact[1] : yact[0];
      resizeGL();
    }
    plot();
  } else if (panning) {
    panning= false;
    xact[1]= me->x();
    yact[1]= height() - me->y();
    pos2map(xact[1],yact[1]);
    float dx= xact[0]-xact[1];
    float dy= yact[0]-yact[1];
    x1map= x1map+dx;
    x2map= x2map+dx;
    y1map= y1map+dy;
    y2map= y2map+dy;
    resizeGL();
    plot();
  }
  setMouseTracking(false);
  useraction= false;
}


void GLwidget::pos2map(float &x, float &y)
{
  x= x1tot + (x2tot-x1tot) * x/float(plotw);
  y= y1tot + (y2tot-y1tot) * y/float(ploth);
}


void GLwidget::draw()
{
//#######################################################
//cerr << "GLwidget::draw" << endl;
//#######################################################

  if (!useraction && !endOfRun) {
    if (!pause || onestep) {
//#######################################################
//cerr << "............snap" << endl;
//#######################################################
      // call fortran (compute next step or whatever to be done)
      int iaction= 1;
      int iexit= 0;
      snap_(&iaction, &iexit);
      if (iexit==1) {
        endOfRun= true;
      } else if (iexit!=0) {
        cerr << "ERROR EXIT" << endl;
        //qApp->quit();
        endOfRun= true;
      }
      onestep= false;
    }
  }

  int nfields,nx,ny,mpos,istep,nstep;
  int year,month,day,hour,minute;
  int numreleased,numrunning;
  int isavePNG,isaveXPM,isaveBMP;
  getdrawspec_(&nfields, &nx, &ny, &mpos, &istep, &nstep,
	       &year, &month, &day, &hour, &minute,
	       &numreleased, &numrunning,
	       &isavePNG, &isaveXPM, &isaveBMP);

  savePNG= (isavePNG>0);
  saveXPM= (isaveXPM>0);
  saveBMP= (isaveBMP>0);

  if (!fp) {
    fp= new FontManager();
    fp->parseSetup();
    fp->setFont("BITMAPFONT");
    fp->setFontFace(glText::F_NORMAL);
    fp->setScalingType(glText::S_FIXEDSIZE);
  }

  fp->setVpSize(float(plotw),float(ploth));
  fp->setGlSize(x2tot-x1tot,y2tot-y1tot);

  // white background
  glClearColor(1.,1.,1.,0.0);
  glClear( GL_COLOR_BUFFER_BIT );

/**********************************************************
  if (nfields>0) {

    float *field= new float[nx*ny];

    for (int i=0; i<nfields; i++) {
      int nfield= i+1;
      int fieldid= 0;
      getfield_(&nfield,&fieldid,field);

      switch (fieldid) {
	case:

          break;
        default:
          cerr << "BAD FIELDID: " << fieldid << endl;
      }

    }

    delete[] field;
  }
**********************************************************/

/**********************************************************/
  float xymap[4]= { x1map, x2map, y1map, y2map };
  float *field= new float[nx*ny];
  int nfield= 1;
  int fieldid= 0;
  miString pspec;
/**********************************************************/

/**********************************************************/
  fieldid= -1;  // total deposition, all components
  getfield_(&nfield,&fieldid,field);
  plotAlpha_shade(nx, ny, field, xymap);
/**********************************************************/

  // show map
  map->plot();

/**********************************************************/
  fieldid= 58;  // MSLP
  getfield_(&nfield,&fieldid,field);
  pspec= "lineinterval=5 linecolour=darkBlue";
  plotContour(nx, ny, field, pspec, fp, xymap);
/**********************************************************/

/**********************************************************/
  fieldid= 17;  // precipitation (mm/hour)
  getfield_(&nfield,&fieldid,field);
  pspec= "linevalues=0.2,0.5,1,2,4,6,10,15,20,25,30,40,50";
  pspec+=" linecolour=darkRed";
  plotContour(nx, ny, field, pspec, fp, xymap);
/**********************************************************/

/**********************************************************/
  delete[] field;
/**********************************************************/

  if (mpos>0) {

    float *xpos= new float[mpos];
    float *ypos= new float[mpos];
    int   *ipos= new int[mpos];
    int npos;

    getposis_(&mpos, &npos, xpos, ypos, ipos);

    if (npos>0) {

      Colour c0= Colour("black");
      Colour c1= Colour("red");

      float col[2][4] = { c0.fR(), c0.fG(), c0.fB(), c0.fA(),
			  c1.fR(), c1.fG(), c1.fB(), c1.fA() };

      glBegin(GL_POINTS);

      int ic= -1;

      float x1= x1map + 1.;
      float x2= x2map + 1.;
      float y1= y1map + 1.;
      float y2= y2map + 1.;

      for (int i=0; i<npos; i++) {
	if (xpos[i]>x1 && xpos[i]<x2 && ypos[i]>y1 && ypos[i]<y2) {
	  if (ipos[i]!=ic) {
//#######################################################
//	    cerr<<"...............npos,i,ic,ipos[i]: "
//	        <<npos<<" "<<i<<" "<<ic<<" "<<ipos[i]<<endl;
//#######################################################
	    ic= ipos[i];
	    glColor3fv(col[ic]);
          }
          glVertex2f(xpos[i]-1.,ypos[i]-1.);
	}
      }

      glEnd();
    }

    delete[] xpos;
    delete[] ypos;
    delete[] ipos;
  }

  float ytbar= (y1bar+y2bar)*0.5;
  float xtbar= x1bar + (x2bar-x1bar) * float(istep)/float(nstep);
  glLineWidth(6.0);
  glBegin(GL_LINES);
  if (istep>0) {
    glColor3f(1.0,0.0,0.0);
    glVertex2f(x1bar,ytbar);
    glVertex2f(xtbar,ytbar);
  }
  if (istep<nstep) {
    glColor3f(0.0,0.0,1.0);
    glVertex2f(xtbar,ytbar);
    glVertex2f(x2bar,ytbar);
  }
  glEnd();

  // annotation
  float fontsize= 14.;
//fp->set("Arial",glText::F_NORMAL,fontsize);
  fp->set("Helvetica",glText::F_NORMAL,fontsize);
  float chx,chy;
  fp->getCharSize('0',chx,chy);

  miTime t(year,month,day,hour,minute,0);
  miString text= t.isoTime(true,false)
	        + "    " + miString(numreleased)
	        + "  " + miString(numrunning);
  float dx,dy;
  fp->getStringSize(text.cStr(),dx,dy);

  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glColor3f(1.0,1.0,1.0);
  glBegin(GL_POLYGON);
  glVertex2f(x1map+chx*0.5,    y2map-chy*1.25);
  glVertex2f(x1map+dx+chx*2.0, y2map-chy*1.25);
  glVertex2f(x1map+dx+chx*2.0, y2map);
  glVertex2f(x1map+chx*0.5,    y2map);
  glEnd();
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

  glLineWidth(1.0);
  glColor3f(0.0,0.0,1.0);
  glBegin(GL_LINE_LOOP);
  glVertex2f(x1map+chx*0.5,    y2map-chy*1.25);
  glVertex2f(x1map+dx+chx*2.0, y2map-chy*1.25);
  glVertex2f(x1map+dx+chx*2.0, y2map);
  glVertex2f(x1map+chx*0.5,    y2map);
  glEnd();

  fp->drawStr(text.cStr(),x1map+chx,y2map-chy,0.0);

  timestring= t.format("%Y%m%d%H%M");

  if (zooming) {
    glLineWidth(2.0);
    glColor3f(0.3,0.3,0.3);
    glBegin(GL_LINE_LOOP);
    glVertex2f(xact[0],yact[0]);
    glVertex2f(xact[1],yact[0]);
    glVertex2f(xact[1],yact[1]);
    glVertex2f(xact[0],yact[1]);
    glEnd();
  }

//#######################################################
//cerr << "GLwidget::draw end......." << endl;
//#######################################################
}



int main( int argc, char **argv )
{
  //QApplication::setColorSpec( QApplication::CustomColor );
  //QApplication a( argc, argv );

  // transfer arguments to Fortran

  for (int i=1; i<argc; i++) {
    int iset=-i;
    int len1=strlen(argv[i]);
    //cerr << "C++ : iset,len1,argv[i]= " << iset << " " << len1 << " " << argv[i] << endl;
    c2fgetarg_(&iset,argv[i],len1);
  }

  // initial call to fortran
  int iaction= 0;
  int iexit= 0;
  snap_(&iaction, &iexit);

  if (iexit!=0) {
    cerr << "ERROR EXIT" << endl;
    return 1;
  }

//  if ( argc >= 2 ) {
//	bool ok = TRUE;
//	timer_interval = QString::fromLatin1( argv[1] ).toInt( &ok );
//	if ( !ok )
//	    timer_interval = 10;
//  }

  int nargc= 0;
  QApplication a( nargc, argv );

  if ( !QGLFormat::hasOpenGL() ) {
    qWarning( "This system has no OpenGL support. Exiting." );
    return -1;
  }

  GLwidget glw;
  a.setMainWidget( &glw );
  glw.show();

  return a.exec();
}
