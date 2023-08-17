local ts = {
  t_mixed = {
    'v_1nokey',
    'v_2nokey',
    a = 'v_a',
    [4] = 'v_4',
    nil,
    [6] = 'v_6',
    nil,
    nil,
    nil,
    nil,
    nil,
    nil,
    nil,
    nil,
    nil,
    [99] = 'v_99',
    [-3] = 'v_3',
    [0] = 'v_0',
  },
  t_list = {
    [9] = 'v_4',
    'v_1',
    'v_2',
    'v_3',
  },
  t_dict = {
    [9] = 'v_4',
    a = 'v_a',
    b = 'v_b',
    c = 'v_c',
  },
}

local function islist(name)
  local t = ts[name]
  local has_item_1 = t[1] ~= nil
  vim.print(('islist: %s: %s'):format(name, has_item_1))
end

islist('t_mixed')
islist('t_list')
islist('t_dict')

local totalstr
local call = 0
local function appendstr(k, v)
  totalstr = totalstr .. ('%s=%s '):format(tostring(k), tostring(v))
end

local function wrap(fnname, fn, tname)
  local t = ts[tname]
  call = call + 1
  totalstr = ('%02d: %-08s: %-08s: '):format(tostring(call), fnname, tname)
  fn(t, appendstr)
  vim.print(totalstr)
end

-- Using pairs() (same as `for k, v in next, t do`):
local function f1(t, fn)
  for k, v in pairs(t) do
    fn(k, v)
  end
end
wrap('pairs', f1, 't_mixed')
wrap('pairs', f1, 't_list')
wrap('pairs', f1, 't_dict')

-- Using ipairs() (equivalent to #):
local function f2(t, fn)
  for i, v in ipairs(t) do
    fn(i, v)
  end
end
wrap('ipairs', f2, 't_mixed')
wrap('ipairs', f2, 't_list')
wrap('ipairs', f2, 't_dict')

-- Using # (numeric for-loop):
local function f3(t, fn)
  for i = 1, #t do
    fn(i, t[i])
  end
end
wrap('#', f3, 't_mixed')
wrap('#', f3, 't_list')
wrap('#', f3, 't_dict')
