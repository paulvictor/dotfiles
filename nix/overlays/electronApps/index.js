const electron = require('electron');
const { session, app, BrowserWindow } = electron;
const path = require('path');

var args = process.argv.slice(2);
var appUrl = args[0];
var appName = args[1];
var userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36";
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
      console.log(ev);
    mainWindow.setTitle(appName);
  });
  mainWindow.loadURL(appUrl, {userAgent: userAgent}).then(() => {
      console.log("loadurl");
      console.log(mainWindow.getTitle());
    mainWindow.setTitle(appName);
  });
  mainWindow.on('closed', () => { mainWindow = null; });
});
