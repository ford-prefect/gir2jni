## TODO

 * Expose `<c:include>` data from GIR so we can emit correct headers for JNI
   (more specifically, expose `girIPIncludes` in `GIRInfo`)

 * Support unions (how?)

 * Support constants: it should be easy to do, but they end up in the global
   namespace, and it'd be nicer to fix them up to be smarter about namespaces.
   For example, `GST_CLOCK_TIME_NONE` in GIR becomes `Gst.CLOCK_TIME_NONE`, but
   what would be nicer is to have it be in the `GstClockTime` class as
   `GstClockTime.NONE`.

 * While Language.C.DSL is a lot easier to use than Language.C, and faster for
   now than writing our own thing, we will probably eventually need to move to
   a pretty printer -- it is not easy to generate human-readable C from an AST,
   and there isn't any non-hacky way to insert macros such as `JNIEXPORT` or
   `#include`.
