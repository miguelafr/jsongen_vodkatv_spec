-module(vodkatv_generators).

-compile(export_all).

gen_integer(Spec,_) ->
    Min = jsg_jsonschema:propertyValue(Spec,"min"),
    Max = jsg_jsonschema:propertyValue(Spec,"max"),
    eqc_gen:choose(Min, Max).
