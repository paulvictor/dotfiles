(in-package :stumpwm)

(require :swank)
(swank-loader:init)

(swank:create-server :port 4005
                     :style swank:*communication-style*
                     :dont-close t)
