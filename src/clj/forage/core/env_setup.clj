(ns "Namespace for global configuration to use one or another environment
    implementation.  Edit this file to switch between env-mason and
    env-matrix."
  env-setup)

(def env 'forage.core.env-mason)
(def env 'forage.core.env-matrix)
