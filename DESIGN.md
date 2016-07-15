## Design

* Read data using Data.GI.CodeGen
  - API
    - Constant
    - Enum
    - Flags
    - Callback
    - Interface
    - Function
    - Object
    - Struct
    - Union

* Output code in the given structure
  - Take top level package prefix (like "org.freedesktop")
  - Take GIR namespace name ("GLib")
  - org.freedesktop.namespace
    - Namespace.java
      - Static namespace-wide things
      - Constant
      - Function
    - Object.java
      - Deriving from GObject (GLib.Object)
      - Property accessors?
      - Signals
        - Actions: normal class methods
        - Regular: listener pattern, add/remove listener methods
    - Interface.java
      - As a Java interface
    - Enum.java, Flags.java
      - As an Java enum
    - Struct.java
      - As a Java class representing the opaque structure
    - Callback.java
      - As a Java interface with an onCallback function
