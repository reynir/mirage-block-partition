## mirage-block-partition -- simple block device partitioning

Stupidly simple block device partitioning.
You give a size of the first partition and as result you get back two devices where the first device has the length you gave and starts from the start of the block device.
The other device returned is the rest of the original block device.

This implementation doesn't care about partition tables and thus there's plenty of opportunity to split the block device at the wrong point.

Interested in using it?
Beware, the interface is almost certain to be changed.
