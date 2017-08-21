import sys
import trace

# create a Trace object, telling it what to ignore, and whether to
# do tracing or line-counting or both.
tracer = trace.Trace(
#    ignoredirs=[sys.prefix, sys.exec_prefix],
    trace=1,
    count=0)

if __name__ == '__main__':
    from yap_kernel import kernelapp as app
    #    tracer.run('app.launch_new_instance()')
    app.launch_new_instance()


