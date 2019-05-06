const electron = require('electron');
const { session, app, BrowserWindow } = electron;
const path = require('path');

var args = process.argv.slice(2);
var appUrl = args[0];
var appName = args[1];
app.on('ready', () => {
  var mainWindow = new BrowserWindow(
  {
    frame: false,
    visible: true,
    webPreferences:
    {
      javascript: true,
      plugins: true,
      partition: "persist:" + appName,
      nodeIntegration: true,
      preload: args[2]
    }
  });
  mainWindow.on("page-title-updated", (ev, title) => {
    mainWindow.setTitle(title);
  });
  mainWindow.loadURL(appUrl,
    {
      userAgent: "Mozilla/5.0 (X11; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0"
    });
  mainWindow.on('closed', () => { mainWindow = null; });
});
