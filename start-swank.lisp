(ql:quickload :swank)

(swank:create-server :port 4005
                     :style swank:*communication-style*
                     :dont-close t)

(ql:quickload :image-board)

(in-package :image-board)

(image-board:start-server)
