const electron = require('electron');
const { session, app, BrowserWindow, Menu, MenuItem, shell } = electron;
const path = require('path');

var args = process.argv.slice(2);
var appUrl = args[0];
var appName = args[1];
var userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36";
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
//       nodeIntegration: true,
//       preload: args[2]
    }
  });
  mainWindow.on("page-title-updated", (ev, title) => {
    console.log(ev);
    mainWindow.setTitle(appName);
  });
  mainWindow.loadURL(appUrl, {userAgent: userAgent}).then(() => {
    mainWindow.setTitle(appName);
  });
  mainWindow.on('closed', () => { mainWindow = null; });

  // Open new-window requests (target="_blank", window.open) in the system browser
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });

  // Keyboard shortcuts
  mainWindow.webContents.on('before-input-event', (event, input) => {
    if (input.type !== 'keyDown') return;
    if (input.key === 'F12') {
      mainWindow.webContents.toggleDevTools();
      event.preventDefault();
    } else if (input.control) {
      if (input.key === 'r') {
        mainWindow.webContents.reload();
        event.preventDefault();
      } else if (input.key === '=') {
        mainWindow.webContents.setZoomLevel(mainWindow.webContents.getZoomLevel() + 0.5);
        event.preventDefault();
      } else if (input.key === '-') {
        mainWindow.webContents.setZoomLevel(mainWindow.webContents.getZoomLevel() - 0.5);
        event.preventDefault();
      } else if (input.key === '0') {
        mainWindow.webContents.setZoomLevel(0);
        event.preventDefault();
      }
    }
  });

  mainWindow.webContents.on('context-menu', (event, params) => {
    const menu = new Menu();

    if (params.selectionText) {
      menu.append(new MenuItem({ label: 'Copy', role: 'copy' }));
    }
    if (params.isEditable) {
      menu.append(new MenuItem({ label: 'Cut', role: 'cut' }));
      menu.append(new MenuItem({ label: 'Paste', role: 'paste' }));
    }
    if (params.linkURL) {
      menu.append(new MenuItem({
        label: 'Copy Link',
        click: () => { electron.clipboard.writeText(params.linkURL); }
      }));
    }
    menu.append(new MenuItem({ type: 'separator' }));
    menu.append(new MenuItem({ label: 'Back', click: () => mainWindow.webContents.goBack() }));
    menu.append(new MenuItem({ label: 'Forward', click: () => mainWindow.webContents.goForward() }));
    menu.append(new MenuItem({ label: 'Reload', role: 'reload' }));

    menu.popup({ window: mainWindow });
  });
});
