---Functions
local function totable(o)
    if type(o) == "table" then
        return o
    end
    if type(o) == "string" then
        local t = {}
        for i = 1, #o do
            table.insert(t, o:sub(i,i))
        end
        return t
    end
end
toString = tostring
tostring = function(val)
    if type(val) ~= "table" then return toString(val) end
    if val then
        if val.repr then return val.repr(val) end
        local str = "{ "
        for k, v in pairs(val) do
            if type(v) == "string" then
                str = str .. k .. ": " .. '"' .. v .. '", '
            elseif type(v) == "table" then
                if v.repr then
                    str = str .. k .. ": " .. v.repr(v) .. ", "
                else
                    str = str .. k .. ": " .. tostring(v) .. ", "
                end
            else
                str = str .. k .. ": " .. tostring(v) .. ", "
            end
        end
        return str:sub(1, #str-2).." }"
    else
        return "{}"
    end
end
table.containsKey = function(t, k) for key, _ in pairs(t) do if key == k then return true end end return false end
table.contains = function(t, v)
    for _, val in pairs(t) do
        if val == v then return true end
        if type(val) == "table" then if table.equal(val, v) then return true end end
    end
    return false
end
table.equal = function(t1, t2)
    for k, v in pairs(t1) do
        if table.contains(t2, v) and table.containsKey(t2, k) then
            return true
        end
    end
    return false
end
table.keyOfValue = function(t, v)
    for k, val in pairs(t) do
        if val == v then return k end
    end
    return nil
end
local function file_exists(file) local f = io.open(file, "rb"); if f then f:close() end return f ~= nil end
local function lines_from(file) if not file_exists(file) then return error("file doesn't exist", 2) end local lines = {} for line in io.lines(file) do lines[#lines + 1] = line end return lines end
string.join = function(s, t)
    local str = ""
    local i = 1
    for k, v in pairs(t) do
        if i == #t then str=str..v else str=str..v..s end
        i = i + 1
    end
    return str
end
local function join(...) local str = "" for _, v in ipairs(arg) do str = str .. tostring(arg) end return str end
---Grammar
local T = {
    num = "number", str = "string", null = "null", bool = "bool", name = "name",
    keyword = "keyword", plus = "plus", minus = "minus", mul = "mul", div = "div", divint = "divint", pow = "pow", index = "index", mod = "mod",
    eq = "eq", rep = "rep", sep = "sep", nl = "nl", eof = "eof",
    evalin = "evalin", evalout = "evalout", listin = "listin", listout = "listout", tablein = "tablein", tableout = "tableout",
    not_ = "not", ee = "ee", ne = "ne", lt = "lt", gt = "gt", lte = "lte", gte = "gte",
    and_ = "and", or_ = "or",
}
local S = {
    [";"] = T.nl, ["\n"] = T.nl, ["="] = T.eq, [":"] = T.rep, [","] = T.sep,
    ["("] = T.evalin, [")"] = T.evalout, ["["] = T.listin, ["]"] = T.listout, ["{"] = T.tablein, ["}"] = T.tableout,
    ["+"] = T.plus, ["-"] = T.minus, ["*"] = T.mul, ["**"] = T.pow, ["/"] = T.div, ["//"] = T.divint, ["."] = T.index, ["%"] = T.mod,
    ["!"] = T.not_, ["=="] = T.ee, ["!="] = T.ne, ["<"] = T.lt, [">"] = T.gt, ["<="] = T.lte, [">="] = T.gte,
    ["&"] = T.and_, ["|"] = T.or_,
}
local K = {
    valDef = "val", constDef = "const", typeDef = "type",
    ["if"] = "if", ["else"] = "else", elif = "elif", ["while"] = "while", ["for"] = "for", switch = "switch", case = "case", default = "default",
    ["then"] = "then", ["do"] = "do", ["end"] = "end",
    ["in"] = "in", of = "of",
}
local CHARS = { "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u",
                "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_" }
local NUMBERS = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" }
local STRDEF = '"'
local COMMENT = "#"
---Lexer
local function Position(idx, ln, col, fn, ftext)
    return { idx = idx, ln = ln, col = col, fn = fn, ftext = ftext,
             next = function(self, char)
                 self.idx = self.idx + 1
                 self.col = self.col + 1
                 if char == "\n" then
                     self.ln = self.ln + 1
                     self.col = 0
                 end
                 return self
             end,
             repr = function(self) return "<pos: i="..tostring(self.idx)..", ln="..tostring(self.ln)..", col="..tostring(self.col)..", fn=\""..tostring(self.fn).."\">" end
    }
end
local function Token(type, value, pos_start, pos_end)
    return { type = type, value = value, pos_start = pos_start, pos_end = pos_end,
             matches = function(self, token) return self.type == token.type and self.value == token.value end,
             repr = function(self) if self.value ~= nil then return "("..self.type..", "..tostring(self.value)..")" else return "("..self.type..")" end end
    }
end
local function lex(fn, text)
    local pos = Position(0, 0, 0, fn, text)
    local char = " "
    local function advance()
        pos = pos.next(pos, char)
        if pos.idx < #text + 1 then
            char = text:sub(pos.idx, pos.idx)
        else
            char = nil
        end
    end
    advance()
    local tokens = {}
    while char do
        if table.containsKey(S, char) then
            local s = char
            advance()
            if table.containsKey(S, s..char) then
                table.insert(tokens, Token(S[s..char], nil, pos, pos))
                advance()
            else
                table.insert(tokens, Token(S[s], nil, pos, pos))
            end
        elseif table.contains(CHARS, char) then
            local start = pos
            local name = ""
            while table.contains(CHARS, char) or table.contains(NUMBERS, char) do
                name = name..char
                advance()
            end
            if table.contains(K, name) then
                table.insert(tokens, Token(T.keyword, name, start, pos))
            elseif name == "true" or name == "false" then
                table.insert(tokens, Token(T.bool, (name == "true"), start, pos))
            elseif name == "null" then
                table.insert(tokens, Token(T.null, nil, start, pos))
            else
                table.insert(tokens, Token(T.name, name, start, pos))
            end
        elseif table.contains(NUMBERS, char) then
            local start = pos
            local number = ""
            while table.contains(NUMBERS, char) or char == "." do
                number = number..char
                advance()
            end
            table.insert(tokens, Token(T.num, tonumber(number), start, pos))
        elseif char == STRDEF then
            local start = pos
            advance()
            if char == '"' then
                table.insert(tokens, Token(T.str, "", start, pos))
                advance()
            else
                local str = char
                advance()
                while char ~= '"' do
                    str = str .. char
                    advance()
                end
                table.insert(tokens, Token(T.str, str, start, pos))
                advance()
            end
        elseif char == COMMENT then
            while char ~= "\n" and char do advance() end
        else
            advance()
        end
    end
    table.insert(tokens, Token(T.eof, nil, pos, pos))
    return tokens
end
---Parser
function ValAssignNode(name_tok, expr)
    return { type = "varAssignNode", name_tok = name_tok, expr = expr, pos_start = name_tok.pos_start, pos_end = expr.pos_end,
             repr = function(self) return "("..self.type.." "..tostring(self.name_tok).." "..tostring(self.expr)..")" end
    }
end
function BinOpNode(left, op_tok, right)
    return { type = "binOpNode", left = left, op_tok = op_tok, right = right, pos_start = left.pos_start, pos_end = right.pos_end,
             repr = function(self) return "("..self.type.." "..tostring(self.left).." "..tostring(self.op_tok).." "..tostring(self.right)..")" end
    }
end
function UnaryOpNode(op_tok, node)
    return { type = "unaryOpNode", node = node, op_tok = op_tok, pos_start = op_tok.pos_start, pos_end = node.pos_end,
             repr = function(self) return "("..self.type.." "..tostring(self.op_tok).." "..tostring(self.node)..")" end
    }
end
function NumberNode(number_tok)
    return { type = "numberNode", number_tok = number_tok, pos_start = number_tok.pos_start, pos_end = number_tok.pos_end,
             repr = function(self) return "("..self.type.." "..tostring(self.number_tok)..")" end
    }
end
function BoolNode(bool_tok)
    return { type = "boolNode", bool_tok = bool_tok, pos_start = bool_tok.pos_start, pos_end = bool_tok.pos_end,
             repr = function(self) return "("..self.type.." "..tostring(self.bool_tok)..")" end
    }
end
function StringNode(string_tok)
    return { type = "stringNode", string_tok = string_tok, pos_start = string_tok.pos_start, pos_end = string_tok.pos_end,
             repr = function(self) return "("..self.type.." \""..tostring(self.string_tok).."\")" end
    }
end
function NullNode(null_tok)
    return { type = "nullNode", null_tok = null_tok, pos_start = null_tok.pos_start, pos_end = null_tok.pos_end,
             repr = function(self) return "("..self.type..")" end
    }
end
local function parse(tokens)
    local tok
    local tok_idx = 0
    local function update_tok()
        if tok_idx >= 0 and tok_idx < #tokens then
            tok = tokens[tok_idx]
        end
    end
    local function advance()
        tok_idx = tok_idx + 1
        update_tok()
    end
    advance()
    local bin_op
    local atom
    local index
    local call
    local power
    local factor
    local term
    local arith_expr
    local comp_expr
    local expr
    local statement
    local statements
    bin_op =  function (func1, ops, func2)
        if not func2 then func2 = func1 end
        local left, err = func1() if err then return nil, err end
        while table.contains(ops, tok.type) or table.contains(ops, { tok.type, tok.value }) do
            local op_tok = tok
            advance()
            local right
            right, err = func2() if err then return nil, err end
            local res = BinOpNode(left, op_tok, right)
            return res
        end
        return left
    end
    atom = function()
        local tok_ = tok
        if tok.type == T.num then
            advance()
            return NumberNode(tok_)
        end
        if tok.type == T.bool then
            advance()
            return BoolNode(tok_)
        end
        if tok.type == T.str then
            advance()
            return StringNode(tok_)
        end
        if tok.type == T.null then
            advance()
            return NullNode(tok_)
        end
        if tok.type == T.evalin then
            advance()
            local node, err = expr()
            advance()
            return node, err
        end
        return nil, 'ERROR: expected number, bool, string, "("'
    end
    index = function()
        local node, err = bin_op(atom, { T.index }, index)
        return node, err
    end
    call = function()  end
    power = function()
        local node, err = bin_op(index, { T.pow, T.mod }, factor)
        return node, err
    end
    factor = function()
        local op = tok
        if op.type == T.minus then
            advance()
            local node, err = factor() if err then return nil, err end
            return UnaryOpNode(op, node)
        end
        local node, err = power()
        return node, err
    end
    term = function()
        local node, err = bin_op(factor, { T.mul, T.div, T.divint}, term)
        return node, err
    end
    arith_expr = function()  end
    comp_expr = function()  end
    expr = function()
        local node, err = bin_op(term, { T.plus, T.minus }, expr)
        return node, err
    end
    statement = function()
        local node, err = expr() if err then return nil, err end
        return node
    end
    statements = function()
        local statement_, err = statement()
        return statement_, err
    end
    local ast, err = statements() if err then return nil, err end
    return ast, nil
end
---Interpreter
function Value()
    return { tonum = function(self) return nil end, tostr = function(self) return nil end,
             repr = function(self)
                 local str = "("
                 if self.type then
                     str = str..self.type
                     if self.value then
                         str = str..": "..tostring(self.value)
                     end
                 end
                 return str..")"
             end,
             add = function(self, other, RETURN)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then right = other.tonum(other) end
                     if right then return Number(left.value + right.value), nil
                     else return nil, "ERROR: cannot cast "..other.type.." to number" end
                 elseif self.type == "string" then
                     local left = self
                     local right
                     if other.tostr then right = other.tostr(other) end
                     if right then return String(left.value .. right.value), nil
                     else return nil, "ERROR: cannot cast "..other.type.." to string" end
                 else
                     if RETURN then return nil, "ERROR: cannot add "..other.type.." with "..self.type end
                     local value, err = other.add(other, self, true)
                     return value, err
                 end
             end,
             sub = function(self, other, RETURN)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then right = other.tonum(other) end
                     if right then return Number(left.value - right.value), nil
                     else return nil, "ERROR: cannot cast "..other.type.." to number" end
                 else
                     if RETURN then return nil, "ERROR: cannot sub "..other.type.." with "..self.type end
                     local value, err = other.sub(other, self, true)
                     return value, err
                 end
             end,
             mul = function(self, other, RETURN)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then right = other.tonum(other) end
                     if right then return Number(left.value * right.value), nil
                     else return nil, "ERROR: cannot cast "..other.type.." to number" end
                 else
                     if RETURN then return nil, "ERROR: cannot sub "..other.type.." with "..self.type end
                     local value, err = other.sub(other, self, true)
                     return value, err
                 end
             end,
             div = function(self, other, RETURN)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then right = other.tonum(other) end
                     if right then
                         if left.value == 0 then
                             return Number(0), nil
                         else
                             return Number(left.value / right.value), nil
                         end
                     else return nil, "ERROR: cannot cast "..other.type.." to number" end
                 else
                     if RETURN then return nil, "ERROR: cannot div "..other.type.." with "..self.type end
                     local value, err = other.div(other, self, true)
                     return value, err
                 end
             end,
             divint = function(self, other, RETURN)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then right = other.tonum(other) end
                     if right then return Number(left.value // right.value), nil
                     else return nil, "ERROR: cannot cast "..other.type.." to number" end
                 else
                     if RETURN then return nil, "ERROR: cannot div "..other.type.." with "..self.type end
                     local value, err = other.divint(other, self, true)
                     return value, err
                 end
             end,
    }
end
function Number(number)
    number = tonumber(number)
    local class = Value()
    class.type = "number"
    class.tonum = function(self) return self end
    class.tostr = function(self) return String(tostring(self.value)) end
    class.str = function(self) return tostring(self.value) end
    if math.floor(number) == number then class.value = math.floor(number) else class.value = number end
    return class
end
function Bool(bool)
    local class = Value()
    class.type = "bool"
    class.tonum = function(self) if self.value then return Number(1) else return Number(0) end end
    class.tostr = function(self) return String(tostring(self.value)) end
    class.str = function(self) return tostring(self.value) end
    if bool then class.value = true else class.value = false end
    return class
end
function Null()
    local class = Value()
    class.type = "null"
    class.tonum = function(self) return Number(0) end
    class.tostr = function(self) return String("null") end
    class.str = function(self) return "null" end
    return class
end
function String(str)
    local class = Value()
    class.type = "string"
    class.tonum = function(self)
        for _, c in pairs(totable(self.value)) do
            if not table.contains(NUMBERS, c) then return nil end
        end
        return Number(tonumber(self.value))
    end
    class.tostr = function(self) return String(tostring(self.value)) end
    class.value = tostring(str)
    class.str = function(self) return self.value end
    return class
end
function List(list)
    local class = Value()
    class.type = "list"
    class.value = list
    return class
end
local function interpret(ast, global_context)
    local visit = {
        binOpNode = function(self, node, context)
            local left
            local op_tok = node.op_tok
            local right
            local value
            local err
            left, err = self[node.left.type](self, node.left, context) if err then return nil, err end
            right, err = self[node.right.type](self, node.right, context) if err then return nil, err end
            if op_tok.type == T.plus then if left.add then
                value, err = left.add(left, right) if err then return nil, err end
            else return nil, "ERROR: cannot add "..tostring(left.type) end end
            if op_tok.type == T.minus then if left.sub then
                value, err = left.sub(left, right) if err then return nil, err end
            else return nil, "ERROR: cannot subtract "..tostring(left.type) end end
            if op_tok.type == T.mul then if left.mul then
                value, err = left.mul(left, right) if err then return nil, err end
            else return nil, "ERROR: cannot multiply "..tostring(left.type) end end
            if op_tok.type == T.div then if left.div then
                value, err = left.div(left, right) if err then return nil, err end
            else return nil, "ERROR: cannot divide "..tostring(left.type) end end
            if op_tok.type == T.divint then if left.divint then
                value, err = left.divint(left, right) if err then return nil, err end
            else return nil, "ERROR: cannot integer-divide "..tostring(left.type) end end
            if value then return value
            else return nil, "ERROR: "..op_tok.type.." is not a valid binary operator" end
        end,
        unaryOpNode = function(self, node, context)
            local value, err = self[node.node.type](self, node.node, context) if err then return nil, err end
            return Number(-value.value)
        end,
        numberNode = function(self, node, context)
            return Number(node.number_tok.value)
        end,
        boolNode = function(self, node, context)
            return Bool(node.bool_tok.value)
        end,
        stringNode = function(self, node, context)
            return String(node.string_tok.value)
        end,
        nullNode = function() return Null() end
    }
    local value, err = visit[ast.type](visit, ast, global_context)
    return value, err
end
---Execution
local fn
local text
if arg[1] then text = string.join("\n", lines_from(arg[1])); fn = arg[1] else text = string.join("\n", lines_from(arg[0]:sub(1, #arg[0]-8).."test.o")); fn = "test.o" end
local tokens = lex(fn, text) if tokens[1].type == T.eof then return end
local ast, err = parse(tokens) if err then print(err) return end
print(ast)
local value; value, err = interpret(ast, {}) if err then print(err) else if value then if value.str then print(value.str(value)) end end end