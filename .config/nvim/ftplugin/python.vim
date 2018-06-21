let b:undo_ftplugin = 'setlocal copyindent< formatprg<'

setlocal copyindent
setlocal formatprg=python\ -m\ autopep8\ -

let b:printf_pattern = "logging.info('%s'.format(%s))"
