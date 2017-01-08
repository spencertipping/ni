u"ni.scheme:ni.scheme"->create('ni.scheme',
  name        => 'URI scheme describing URI schemes',
  synopsis    => q{ u"ni.scheme:http"->u("google.com")
                  | u"ni.scheme:ni.rmi.pid"->behaviors },
  description => q{
    This is the fixed-point definition at the core of ni's meta-object
    protocol.

    TODO explain this more});
