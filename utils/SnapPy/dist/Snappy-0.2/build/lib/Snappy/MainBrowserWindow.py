"""
    ********************* MainBrowserWindow ************************

    A simpel Web Browser as interface-window using javascript callbacks
    to get the information back to python

    *******************************************************************
"""

import sys
from PyQt5 import QtCore, QtGui, QtWidgets, QtWebKit, QtWebKitWidgets, QtNetwork
from builtins import str

class StartWebPage(QtWebKitWidgets.QWebPage):
    formSubmitted = QtCore.pyqtSignal(QtCore.QUrl)

    def __init__(self):
        super(StartWebPage, self).__init__()

    def acceptNavigationRequest(self, frame, req, nav_type):
        if nav_type == self.NavigationTypeFormSubmitted:
            # signal event
            self.formSubmitted.emit(req.url())
            return False
        else:
            return super(StartWebPage, self).acceptNavigationRequest(frame, req, nav_type)


class MainBrowserWindow(QtWidgets.QMainWindow):

    def __init__(self):
        """
            Create main window with browser and a button
        """
        super(MainBrowserWindow, self).__init__()

        self.resize(960,1024)
        self.centralwidget = QtWidgets.QWidget(self)

        self.mainLayout = QtWidgets.QHBoxLayout(self.centralwidget)
        self.frame = QtWidgets.QFrame(self.centralwidget)
        self.gridLayout = QtWidgets.QVBoxLayout(self.frame)
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.tb_url = QtWidgets.QLineEdit(self.frame)
        self.bt_back = QtWidgets.QPushButton(self.frame)
        self.bt_ahead = QtWidgets.QPushButton(self.frame)
        self.bt_back.setIcon(QtGui.QIcon().fromTheme("go-previous"))
        self.bt_ahead.setIcon(QtGui.QIcon().fromTheme("go-next"))

        self.bt_back.setHidden(True)
        self.bt_ahead.setHidden(True)
        self.tb_url.setHidden(True)
#         self.horizontalLayout.addWidget(self.bt_back)
#         self.horizontalLayout.addWidget(self.bt_ahead)
#         self.horizontalLayout.addWidget(self.tb_url)
        self.gridLayout.addLayout(self.horizontalLayout)

        self.webview = QtWebKitWidgets.QWebView()
        self.gridLayout.addWidget(self.webview)
        self.mainLayout.addWidget(self.frame)
        self.setCentralWidget(self.centralwidget)

        self.tb_url.returnPressed.connect(self.browse)
        self.bt_back.clicked.connect(self.webview.back)
        self.bt_ahead.clicked.connect(self.webview.forward)
        self.webview.urlChanged.connect(self.url_changed)

        self.set_form_handler(self._default_form_handler)

        #self.default_url = "https://dokit.met.no/fou/kl/prosjekter/eemep/esnap_userdoc"
        #self.tb_url.setText(self.default_url)
        #self.browse()

    def browse(self):
        """browse an url"""
        url = self.tb_url.text() if self.tb_url.text() else self.default_url
        self.webview.setPage(QtWebKitWidgets.QWebPage())
        self.webview.load(QtCore.QUrl(url))
        self.webview.show()


    def url_changed(self, url):
        """ Triggered when the url is changed """
        self.tb_url.setText(url.toString())

    def set_html(self, text: str):
        """ set html string"""
        self.tb_url.setText("")
        self.web_page = StartWebPage()
        self.webview.setPage(self.web_page)
        self.webview.page().formSubmitted.connect(self._handle_formSubmitted)
        self.webview.setHtml(text)

    @staticmethod
    def _default_form_handler(dict):
        for key, value in dict:
            print(str.format("{0} => {1}", key, value))

    def set_form_handler(self, handler):
        """ the form handler should accept a dictionary with query results as input """
        self.form_handler = handler

    def evaluate_javaScript(self, jscript):
        self.webview.page().mainFrame().evaluateJavaScript(jscript);

    def _handle_formSubmitted(self, url):
        # I don't manage to get the right query strings from the web-page
        print("handleFromSubmitted:"+url.toString());
        self.form_handler(QtCore.QUrlQuery(url).queryItems(QtCore.QUrl.FullyDecoded));




if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    main = MainBrowserWindow()
    html = '''
<html>
<body>
<form action="http://example.org" method="get">
Like it?
<input type="radio" name="like" value="yes"/> Yes
<input type="radio" name="like" value="no" /> No
<br/><input type="text" name="text" value="" />
<input type="submit" name="submit" value="Send"/>
</form>
</body>
</html>
'''
    main.set_html(html)
    main.show()
    sys.exit(app.exec_())
