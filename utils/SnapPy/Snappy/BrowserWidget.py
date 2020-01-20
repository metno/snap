# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2017   Norwegian Meteorological Institute
# 
# This file is part of SNAP. SNAP is free software: you can 
# redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the 
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
"""
    ********************* BrowserWidget ************************

    A simpel Web Browser as interface-tab using javascript callbacks
    to get the information back to python

    *******************************************************************
"""

from PyQt5 import QtCore, QtWidgets
try:
    #Qt5.5 and earlier
    from PyQt5.QtWebKitWidgets import QWebPage, QWebView
except:
    #Qt5.6 and later - QtWebKitWidgets is deprecated
    from PyQt5.QtWebEngineWidgets import QWebEnginePage as QWebPage
    from PyQt5.QtWebEngineWidgets import QWebEngineView as QWebView
    
from builtins import str
import sys


class StartWebPage(QWebPage):
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


class BrowserWidget(QtWidgets.QWidget):

    def __init__(self):
        """
            Create main window with browser and a button
        """
        super(BrowserWidget, self).__init__()

        self.layout = QtWidgets.QHBoxLayout()
        self.setLayout(self.layout)

        self.webview = QWebView()
        self.layout.addWidget(self.webview)

        self.webview.urlChanged.connect(self.url_changed)

        self.set_form_handler(self._default_form_handler)

        #self.default_url = "https://dokit.met.no/fou/kl/prosjekter/eemep/esnap_userdoc"
        #self.tb_url.setText(self.default_url)
        #self.browse()

    def browse(self):
        """browse an url"""
        url = self.tb_url.text() if self.tb_url.text() else self.default_url
        self.webview.setPage(QWebPage())
        self.webview.load(QtCore.QUrl(url))
        self.webview.show()


    def url_changed(self, url):
        """ Triggered when the url is changed """

    def set_html(self, text: str):
        """ set html string"""
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

    tab1 = BrowserWidget()
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
    tab1.set_html(html)
    tabs = QtWidgets.QTabWidget()
    tabs.addTab(tab1, 'Test')
    tabs.resize(960,1024)
    tabs.show()
    sys.exit(app.exec_())
