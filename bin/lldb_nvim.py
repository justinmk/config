import lldb
import os

def runnvim(debugger, command, exe_ctx, result, internal_dict):
    target = debugger.GetSelectedTarget()
    if target:
        process = target.GetProcess()
        if not process.IsValid():  # Check if a process is *already* associated
            # If not, create the process (launch nvim).
            launch_info = target.GetLaunchInfo()
            executable = target.GetExecutable()
            if executable.IsValid():
                launch_info.SetExecutableFile(executable, True)
            else:
                print('Error: Invalid executable found')
                return

            # Set arguments.
            launch_info.SetArguments(['--luamod-dev'], False)

            # Set env vars.
            environment = lldb.SBEnvironment()
            environment.Set('VIMRUNTIME', os.path.expanduser('~/dev/neovim/runtime/'), True)
            launch_info.SetEnvironment(environment, True)

            error = lldb.SBError()
            process = debugger.GetSelectedTarget().Launch(launch_info, error)

            if error.Fail():
                print(f'Process launch failed: {error.GetCString()}')
                return

            # print("nvim launched")

        # Now that the process is guaranteed to exist, set the breakpoint.
        bp = target.BreakpointCreateByName('os_exit')
        if not bp.IsValid():
            print('Failed to set breakpoint.')

        process.Continue()

    else:
        print('No target found')
