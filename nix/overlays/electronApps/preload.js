window.navigator.serviceWorker.getRegistrations().then(registrations => {
  for (let registration of registrations) {
      registration.unregister(); //Unregisters all the service workers
      window.location.reload();
  }
})
