Default values for an attribute are :
Class: AttrUnknown
Ignored: true if Class = AttrUnknown else false
Printed: true

Create a new attribute and register it with default values.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:default
  [kernel:attrs] 
    Registering attribute "myattr" with information
    Class: AttrUnknown, Ignored: true, Printed: true

Override default values, not printed anymore.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:noprint
  [kernel:attrs] 
    Registering attribute "myattr" with information
    Class: AttrUnknown, Ignored: true, Printed: false

Override default values, the class is AttrType and not ignored anymore.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:type
  [kernel:attrs] 
    Registering attribute "myattr" with information
    Class: AttrType, Ignored: false, Printed: true

Using the same key several time can override the previous value. The order is unspecified.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:noprint,myattr:print
  [kernel:attrs] 
    Registering attribute "myattr" with information
    Class: AttrUnknown, Ignored: true, Printed: false

Registering 2 attributes.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr1:name,myattr2:noprint,myattr1:noprint
  [kernel:attrs] 
    Registering attribute "myattr1" with information
    Class: AttrName false, Ignored: false, Printed: false
  [kernel:attrs] 
    Registering attribute "myattr2" with information
    Class: AttrUnknown, Ignored: true, Printed: false

Replacing an existing attribute with default values changes nothing, because
default takes the old information.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes dummy:default
  [kernel:attrs] 
    Attribute "dummy" already registered with information Class: AttrUnknown, Ignored: true, Printed: true. Nothing to do

Replacing an existing attribute.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes dummy:type,dummy:noignore,dummy:noprint
  [kernel:attrs] 
    Replacing existing class and status for attribute dummy:
    was (Class: AttrUnknown, Ignored: true, Printed: true),
    now (Class: AttrType, Ignored: false, Printed: false)

Default value associated with anything else has no effect.
myattr:default,myattr:name <==> myattr:name,myattr:default <==> myattr:name
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:default,myattr:name
  [kernel:attrs] 
    Registering attribute "myattr" with information
    Class: AttrName false, Ignored: false, Printed: true

Each key needs to be bound to at least one element, which is why default exists.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:
  [kernel] User Error: incorrect argument for option -register-attributes
    (no value bound to 'myattr:').
  [kernel] Frama-C aborted: invalid user input.
  [1]

Using an unknown value is not allowed.
  $ frama-c -no-autoload-plugins -kernel-msg-key attrs -register-attributes myattr:toto
  [kernel] User Error: incorrect argument for option -register-attributes
    (value bound to 'myattr': unknown attribute info "toto").
  [kernel] Frama-C aborted: invalid user input.
  [1]
