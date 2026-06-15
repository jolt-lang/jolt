# Host interop aggregator (jolt-jx5l, phase 1 subdir split).
#
# The JVM class/method shim surface that portable cljc libraries call now lives in
# src/jolt/interop/ split by area: java_base (java.lang statics + java.time +
# shared coercion helpers), host_io (java.io/util/net/sql/text), collections
# (the late-bound .iterator/.nth/... hooks over jolt values). The registry
# MACHINERY (class-statics / tagged-methods / register-*!) still lives in the
# evaluator, which loads before this module — so the registries exist before any
# install! runs here.
#
# api imports this module (import ./host_interop) to trigger registration at load.
# Adding a JDK-area shim is now a one-file change under interop/ plus a line here.
(use ./interop/java_base)
(use ./interop/host_io)
(use ./interop/collections)

(install!)
(install-io!)
(install-collections!)
