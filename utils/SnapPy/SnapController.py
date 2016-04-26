import sys
from MainBrowserWindow import MainBrowserWindow
from Resources import Resources
from PyQt5 import QtWidgets, QtGui

class SnapController:
    def __init__(self):
        self.res = Resources()
        self.main = MainBrowserWindow()
        self.main.setWindowTitle('SNAPpy')
        self.main.setWindowIcon(QtGui.QIcon(self.res.getIconPath()))
        self.main.set_html(self.res.getStartScreen())
        self.main.set_form_handler(self._create_snap_form_handler())
        self.main.show()

    def run_snap_query(self, qDict):
        MainBrowserWindow._default_form_handler(qDict)
        self.main.evaluate_javaScript('updateSnapLog("{0}");'.format("working..."))

    def update_log_query(self, qDict):
        MainBrowserWindow._default_form_handler(queryDict)
        self.main.evaluate_javaScript('updateSnapLog("{0}");'.format("updating..."))


    def _create_snap_form_handler(self):
        def handler(queryDict):
            """a form-handler with closure for self"""
            options = { 'Run' : self.run_snap_query,
                        'Update' : self.update_log_query
            }
            # mapping from QList<QPair> to simple dictionary
            qDict = dict()
            for key, value in queryDict:
                qDict[key] = value
            # calling the correct handler depending on the module
            try:
                options[qDict['action']](queryDict)
            except:
                print("Unexpected error: {}".format(sys.exc_info()[0]))
                self.main.evaluate_javaScript("Unexpected error on {0}: {1}".format(qDict['action'],sys.exc_info()[0]))
        return handler


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    ctr = SnapController()
    sys.exit(app.exec_())

