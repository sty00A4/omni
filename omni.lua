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
    local equal = true
    for k, v in pairs(t1) do
        if not (table.contains(t2, v) and table.containsKey(t2, k)) then
            equal = false
        end
    end
    return equal
end
table.keyOfValue = function(t, v)
    for k, val in pairs(t) do
        if val == v then return k end
    end
    return nil
end
table.sub = function(t, i, j)
    if not j then j = #t end
    local st = {}
    for idx, v in ipairs(t) do
        if idx >= i and idx <= j then
            table.insert(st, v)
        end
    end
    return st
end
table.extend = function(t1, t2)
    for _,v in ipairs(t2) do
        table.insert(t1, v)
    end
    return t1
end
local function file_exists(file) local f = io.open(file, "rb"); if f then f:close() end return f ~= nil end
local function lines_from(file) if not file_exists(file) then return error("file doesn't exist", 2) end local lines = {} for line in io.lines(file) do lines[#lines + 1] = line end return lines end
string.join = function(s, t)
    local str = ""
    local i = 1
    for k, v in pairs(t) do
        if i == #t then str=str..tostring(v) else str=str..tostring(v)..s end
        i = i + 1
    end
    return str
end
string.split = function(s, sep)
    local t = {}
    local temp = ""
    for _, c in pairs(totable(s)) do
        if c == sep then if #temp > 0 then table.insert(t, temp) temp = "" end
        else temp = temp .. c end
    end
    if #temp > 0 then table.insert(t, temp) temp = "" end
    return t
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
---Errors
local function Error(type, details, pos_start, pos_end)
    return { type = type, details = details, pos_start = pos_start, pos_end = pos_end,
             repr = function(self, text)
                 local str = self.type..": "..self.details
                 if self.pos_start and self.pos_end then
                     if text then str = string.join("\n", table.sub(string.split(text, "\n"), self.pos_start.ln, self.pos_end.ln)).."\n"..str end
                     str = "in \""..self.pos_start.fn .. "\"\n" .. str
                     str = str.." ("..self.pos_start.ln..":"..self.pos_start.col.." to "..self.pos_end.ln..":"..self.pos_end.col..")"
                 end
                 return str
             end,
    }
end
---Lexer
local function Position(idx, ln, col, fn, ftext)
    return { idx = idx, ln = ln, col = col, fn = fn, ftext = ftext,
             copy = function(self) return Position(self.idx, self.ln, self.col, self.fn, self.ftext) end,
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
             repr = function(self)
                 if self.value == nil then
                     return "["..self.type.."]"
                 else
                     return "["..tostring(self.value).."]"
                 end
             end
    }
end
local function lex(fn, text)
    local pos = Position(0, 1, 0, fn, text)
    local char = " "
    local function advance()
        pos.idx = pos.idx + 1
        pos.col = pos.col + 1
        if char == "\n" then
            pos.ln = pos.ln + 1
            pos.col = 0
        end
        if pos.idx < #text + 1 then
            char = text:sub(pos.idx, pos.idx)
        else
            char = nil
        end
    end
    advance()
    local tokens = {}
    while char do
        if char == " " or char == "\t" then advance()
        elseif table.containsKey(S, char) then
            local s = char
            advance()
            if char then
                if table.containsKey(S, s..char) then
                    table.insert(tokens, Token(S[s..char], nil, pos.copy(pos), pos.copy(pos)))
                    advance()
                else table.insert(tokens, Token(S[s], nil, pos.copy(pos), pos.copy(pos))) end
            else table.insert(tokens, Token(S[s], nil, pos.copy(pos), pos.copy(pos))) end
        elseif table.contains(CHARS, char) then
            local start = pos.copy(pos)
            local name = ""
            while table.contains(CHARS, char) or table.contains(NUMBERS, char) do
                name = name..char
                advance()
            end
            if table.contains(K, name) then
                table.insert(tokens, Token(T.keyword, name, start.copy(start), pos.copy(pos)))
            elseif name == "true" or name == "false" then
                table.insert(tokens, Token(T.bool, name == "true", start.copy(start), pos.copy(pos)))
            elseif name == "null" then
                table.insert(tokens, Token(T.null, nil, start.copy(start), pos.copy(pos)))
            else
                table.insert(tokens, Token(T.name, name, start.copy(start), pos.copy(pos)))
            end
        elseif table.contains(NUMBERS, char) then
            local start = pos.copy(pos)
            local number = ""
            while table.contains(NUMBERS, char) or char == "." do
                number = number..char
                advance()
            end
            table.insert(tokens, Token(T.num, tonumber(number), start.copy(start), pos.copy(pos)))
        elseif char == STRDEF then
            local start = pos.copy(pos)
            advance()
            if char == '"' then
                table.insert(tokens, Token(T.str, "", start.copy(start), pos.copy(pos)))
                advance()
            else
                local str = char
                advance()
                while char ~= '"' and char do
                    str = str .. char
                    advance()
                end
                table.insert(tokens, Token(T.str, str, start.copy(start), pos.copy(pos)))
                advance()
            end
        elseif char == COMMENT then
            while char ~= "\n" and char do advance() end
        else
            return nil, Error("illegal character error", '"'..char..'"', pos.copy(pos), pos.copy(pos))
        end
    end
    table.insert(tokens, Token(T.eof, nil, pos.copy(pos), pos.copy(pos)))
    return tokens
end
---Parser
function ValCreateNode(name_tok, expr)
    return { type = "valCreateNode", name_tok = name_tok, expr = expr, pos_start = name_tok.pos_start, pos_end = expr.pos_end,
             repr = function(self) return "( val "..tostring(self.name_tok).." "..tostring(self.expr).." )" end
    }
end
function ConstCreateNode(name_tok, expr)
    return { type = "constCreateNode", name_tok = name_tok, expr = expr, pos_start = name_tok.pos_start, pos_end = expr.pos_end,
             repr = function(self) return "( const "..tostring(self.name_tok).." "..tostring(self.expr).." )" end
    }
end
function BinOpNode(left, op_tok, right)
    return { type = "binOpNode", left = left, op_tok = op_tok, right = right, pos_start = left.pos_start, pos_end = right.pos_end,
             repr = function(self) return "( "..tostring(self.left).." "..tostring(self.op_tok).." "..tostring(self.right).." )" end
    }
end
function UnaryOpNode(op_tok, node)
    return { type = "unaryOpNode", node = node, op_tok = op_tok, pos_start = op_tok.pos_start, pos_end = node.pos_end,
             repr = function(self) return "( "..tostring(self.op_tok).." "..tostring(self.node).." )" end
    }
end
function NumberNode(number_tok)
    return { type = "numberNode", number_tok = number_tok, pos_start = number_tok.pos_start, pos_end = number_tok.pos_end,
             repr = function(self) return tostring(self.number_tok) end
    }
end
function BoolNode(bool_tok)
    return { type = "boolNode", bool_tok = bool_tok, pos_start = bool_tok.pos_start, pos_end = bool_tok.pos_end,
             repr = function(self) return tostring(self.bool_tok) end
    }
end
function NullNode(null_tok)
    return { type = "nullNode", null_tok = null_tok, pos_start = null_tok.pos_start, pos_end = null_tok.pos_end,
             repr = function(self) return "null" end
    }
end
function StringNode(string_tok)
    return { type = "stringNode", string_tok = string_tok, pos_start = string_tok.pos_start, pos_end = string_tok.pos_end,
             repr = function(self) return "\""..tostring(self.string_tok).."\"" end
    }
end
function ListNode(list, pos_start, pos_end)
    return { type = "listNode", list = list, pos_start = pos_start, pos_end = pos_end,
             repr = function(self)
                 local str = table.keyOfValue(S, T.listin)
                 for _, v in pairs(self.list) do
                     str = str .. tostring(v) .. table.keyOfValue(S, T.sep)
                 end
                 return str .. table.keyOfValue(S, T.listout)
             end
    }
end
function NameNode(name_tok)
    return { type = "nameNode", name_tok = name_tok, pos_start = name_tok.pos_start, pos_end = name_tok.pos_end,
             repr = function(self) return tostring(self.name_tok) end
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
    local bin_op, list_expr, atom, index, call, power, factor, term, arith_expr, comp_expr, expr, statement, statements
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
    list_expr = function()
        local start = tok.pos_start.copy(tok.pos_start)
        advance() while tok.type == T.nl do advance() end
        local list, node, err = {}
        node, err = expr() if err then return nil, err end
        table.insert(list, node)
        if tok.type == T.listout then table.insert(list, node) return ListNode(list, start.copy(start), tok.pos_end.copy(tok.pos_end)) end
        if tok.type == T.sep then
            while not (tok.type == T.listout) do
                advance() while tok.type == T.nl do advance() end
                if tok.type == T.listout then break end
                node, err = expr() if err then return nil, err end
                table.insert(list, node)
                while tok.type == T.nl do advance() end
                if tok.type == T.listout then break end
                if tok.type ~= T.sep then return nil, Error("invalid syntax", 'expected ",", or "]"', tok.pos_start.copy(tok.pos_start), tok.pos_end.copy(tok.pos_end)) end
            end
            return ListNode(list, start.copy(start), tok.pos_end.copy(tok.pos_end))
        end
        return nil, Error("invalid syntax", 'expected ",", or "]"')
    end
    atom = function()
        local tok_ = tok
        if tok.type == T.name then
            advance()
            return NameNode(tok_)
        end
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
            local node, err = expr() if err then return nil, err end
            if tok.type ~= T.evalout then return nil, Error("invalid syntax", 'expected ")"', tok.pos_start, tok.pos_end) end
            advance()
            return node
        end
        if tok.type == T.listin then
            local node, err = list_expr() if err then return nil, err end
            if tok.type ~= T.listout then return nil, Error("invalid syntax", 'expected ")"', tok.pos_start, tok.pos_end) end
            advance()
            return node
        end
        return nil, Error("invalid syntax", 'expected number, bool, string, "("', tok_.pos_start, tok_.pos_end)
    end
    index = function()
        local node, err = bin_op(atom, { T.index }, index) if err then return nil, err end
        return node
    end
    call = function()  end
    power = function()
        local node, err = bin_op(index, { T.pow, T.mod }, factor) if err then return nil, err end
        return node
    end
    factor = function()
        local op = tok
        if op.type == T.minus then
            advance()
            local node, err = factor() if err then return nil, err end
            return UnaryOpNode(op, node)
        end
        local node, err = power() if err then return nil, err end
        return node
    end
    term = function()
        local node, err = bin_op(factor, { T.mul, T.div, T.divint}, term) if err then return nil, err end
        return node
    end
    arith_expr = function()
        local node, err = bin_op(term, { T.plus, T.minus }, arith_expr) if err then return nil, err end
        return node
    end
    comp_expr = function()
        if tok.matches(tok, Token(T.not_)) then
            local op_tok = tok
            advance()
            local node, err = comp_expr() if err then return nil, err end
            return UnaryOpNode(op_tok, node)
        end
        local node, err = bin_op(arith_expr, { T.ee, T.ne, T.lt, T.gt, T.lte, T.gte }, comp_expr) if err then return nil, err end
        return node
    end
    expr = function()
        local node, err = bin_op(comp_expr, { T.and_, T.or_ }, expr) if err then return nil, err end
        return node
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
    if tokens[tok_idx].type == T.eof then return ast else return nil, Error("run-time error", "no use for expression", tokens[tok_idx].pos_start, tokens[tok_idx].pos_end) end
end
---Interpreter
function Value()
    return { tonum = function(self) return nil end, tostr = function(self) return nil end, tobool = function(self) return nil end,
             repr = function(self)
                 local str = "("
                 if self.value then str = str..tostring(self.value) else str=str..self.type end
                 return str..")"
             end,
             setPos = function(self, pos_start, pos_end) self.pos_start, self.pos_end = pos_start, pos_end return self end,
             add = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Number(left.value + right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 if self.type == "string" then
                     local left = self
                     local right
                     if other.tostr then
                         right = other.tostr(other)
                         if right then return String(left.value .. right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to string", other.pos_start, other.pos_end) end
                     end
                 end
                 if self.type == "list" then
                     if other then
                         local copy = self.copy(self)
                         table.insert(copy.value, other)
                         return List(copy.value)
                     else return nil, Error("cast error", "cannot cast "..other.type.." to string", other.pos_start, other.pos_end) end
                 end
                 return nil, Error("operation error", "cannot do add with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             sub = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Number(left.value - right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 if self.type == "list" then
                     local left = self.copy(self)
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then
                             table.remove(left.value, right.value+1)
                             return List(left.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do subtract with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             mul = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Number(left.value * right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 if self.type == "list" and other.type == "list" then return List(table.extend(self.copy(self).value, other.copy(other).value)) end
                 return nil, Error("operation error", "cannot do multiply with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             div = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then
                             if left.value == 0 then
                                 return Number(0)
                             else
                                 return Number(left.value / right.value)
                             end
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do divide with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             divint = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Number(left.value // right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do integer-divide with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             and_ = function(self, other)
                 if self.type == "bool" then
                     local left = self
                     local right
                     if other.tobool then
                         right = other.tobool(other)
                         if right then return Bool(left.value and right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to bool", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do and-comp with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             or_ = function(self, other)
                 if self.type == "bool" then
                     local left = self
                     local right
                     if other.tobool then
                         right = other.tobool(other)
                         if right then return Bool(left.value or right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to bool", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do or-comp with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             ee = function(self, other) return Bool(self.value == other.value) end,
             ne = function(self, other) return Bool(self.value ~= other.value) end,
             lt = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Bool(left.value < right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do less-than-comp with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             gt = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Bool(left.value > right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do greater-than-comp with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             lte = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Bool(left.value <= right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do less-than-equal with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             gte = function(self, other)
                 if self.type == "number" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then return Bool(left.value >= right.value)
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do greater-than-equal with "..other.type.." and "..self.type, other.pos_start, self.pos_end)
             end,
             index = function(self, other)
                 if self.type == "string" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then
                             local value = left:sub(right.value+1, right.value+1)
                             if value then return value else return nil, Error("index error", "list index out of range", other.pos_start, other.pos_end) end
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 if self.type == "list" then
                     local left = self
                     local right
                     if other.tonum then
                         right = other.tonum(other)
                         if right then
                             local value = left.value[right.value+1]
                             if value then return value else return nil, Error("index error", "list index out of range", other.pos_start, other.pos_end) end
                         else return nil, Error("cast error", "cannot cast "..other.type.." to number", other.pos_start, other.pos_end) end
                     end
                 end
                 return nil, Error("operation error", "cannot do index with "..self.type, other.pos_start, self.pos_end)
             end,
    }
end
function Number(number)
    number = tonumber(number)
    local class = Value()
    class.type = "number"
    class.tonum = function(self) return self end
    class.tostr = function(self) return String(tostring(self.value)) end
    class.tobool = function(self) return Bool(self.value ~= 0) end
    class.str = function(self) return tostring(self.value) end
    if math.floor(number) == number then class.value = math.floor(number) else class.value = number end
    return class
end
function Bool(bool)
    local class = Value()
    class.type = "bool"
    class.tonum = function(self) if self.value then return Number(1) else return Number(0) end end
    class.tostr = function(self) return String(tostring(self.value)) end
    class.tobool = function(self) return self end
    class.str = function(self) return tostring(self.value) end
    if bool then class.value = true else class.value = false end
    return class
end
function Null()
    local class = Value()
    class.type = "null"
    class.tonum = function(self) return Number(0) end
    class.tostr = function(self) return String("null") end
    class.tobool = function(self) return Bool(false) end
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
    class.tobool = function(self) return Bool(#tostring(self.value) > 0) end
    class.value = tostring(str)
    class.str = function(self) return self.value end
    return class
end
function List(list)
    local class = Value()
    class.type = "list"
    class.tobool = function(self) return Bool(#self.list > 0) end
    class.value = list
    class.copy = function(self) return List(self.value) end
    class.str = function(self)
        local str = table.keyOfValue(S, T.listin)
        for _, v in pairs(self.value) do
            str = str .. v.str(v) .. table.keyOfValue(S, T.sep)
        end
        str = str:sub(1, #str-#table.keyOfValue(S, T.sep))
        return str .. table.keyOfValue(S, T.listout)
    end
    return class
end
function Context(variables)
    return { vars = variables,
             get = function(self, name_tok)
                 if not self.vars[name_tok.value] then return Null(), self end
                 return self.vars[name_tok.value].value, self
             end,
             create = function(self, kw, name_tok, init_value)
                 if self.vars[name_tok.value] then return nil, self, Error("name error", "variable is already created", name_tok.pos_start, name_tok.pos_end) end
                 if init_value then self.vars[name_tok.value] = { value = init_value, type = kw } else self.vars[name_tok.value] = { value = Null(), type = kw } end
                 return self.vars[name_tok.value].value, self
             end,
             set = function(self, name_tok, value)
                 if self.vars[name_tok.value].kw == "const" then return nil, self, Error("name error", "cannot alter value of a constant variable", name_tok.pos_start, name_tok.pos_end) end
                 self.vars[name_tok.value].value = value
                 return self.vars[name_tok.value].value, self
             end
    }
end
local function interpret(ast, global_context)
    local visit = {
        binOpNode = function(self, node, context)
            local left
            local op_tok = node.op_tok
            local right
            local value
            local err
            left, context, err = self[node.left.type](self, node.left, context) if err then return nil, context, err end
            right, context, err = self[node.right.type](self, node.right, context) if err then return nil, context, err end
            if op_tok.type == T.plus then if left.add then
                value, err = left.add(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do add on "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.minus then if left.sub then
                value, err = left.sub(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do subtract "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.mul then if left.mul then
                value, err = left.mul(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do multiply "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.div then if left.div then
                value, err = left.div(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do divide "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.divint then if left.divint then
                value, err = left.divint(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do integer-divide "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.or_ then if left.or_ then
                value, err = left.or_(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do or-comp "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.and_ then if left.and_ then
                value, err = left.and_(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do and-comp "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.ee then if left.ee then
                value, err = left.ee(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do equal-comp "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.ne then if left.ne then
                value, err = left.ne(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do not-equal-comp "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.lt then if left.lt then
                value, err = left.lt(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do less-than-comp on "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.gt then if left.gt then
                value, err = left.gt(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do greater-than-comp on "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.lte then if left.lte then
                value, err = left.lte(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do less-than-equal-comp on "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.gte then if left.gte then
                value, err = left.gte(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do greater-than-equal-comp on "..tostring(left.type), left.pos_start, left.pos_end) end end
            if op_tok.type == T.index then if left.index then
                value, err = left.index(left, right) if err then return nil, context, err end
            else return nil, context, Error("operation error", "cannot do index on "..tostring(left.type), left.pos_start, left.pos_end) end end
            if value then return value, context
            else return nil, context, Error("operation error", op_tok.type.." is not a valid binary operator", op_tok.pos_start, op_tok.pos_end) end
        end,
        unaryOpNode = function(self, node, context)
            local value
            local err
            value, context, err = self[node.node.type](self, node.node, context) if err then return nil, context, err end
            if node.op_tok.type == T.minus then
                return Number(-value.value), context
            elseif node.op_tok.type == T.not_ then
                return Bool(not value.value), context
            else
                return nil, context, Error("operation error", "invalid unary operation tok of type "..node.op_tok.type, node.op_tok.pos_start, node.op_tok.pos_end)
            end
        end,
        numberNode = function(self, node, context) local value = Number(node.number_tok.value) return value.setPos(value, node.number_tok.pos_start, node.number_tok.pos_end), context end,
        boolNode = function(self, node, context) local value = Bool(node.bool_tok.value) return value.setPos(value, node.bool_tok.pos_start, node.bool_tok.pos_end), context end,
        stringNode = function(self, node, context) local value = String(node.string_tok.value) return value.setPos(value, node.string_tok.pos_start, node.string_tok.pos_end), context end,
        nullNode = function(self, node, context) local value = Null() return value.setPos(value, node.null_tok.pos_start, node.null_tok.pos_end), context end,
        listNode = function(self, node, context)
            local list, value, err = {}
            for _, v in pairs(node.list) do
                value, context, err = self[v.type](self, v, context) if err then return nil, context, err end
                table.insert(list, value)
            end
            return List(list), context
        end,
        nameNode = function(self, node, context)
            local value, err
            value, context, err = context.get(context, node.name_tok) if err then return nil, context, err end
            return value, context
        end,
        valCreateNode = function(self, node, context)
            local value
            local err
            if node.expr then value, context, err = self[node.expr.type](self, node.expr, context) if err then return nil, context, err end end
            value, context, err = context.create(context, "val", node.name_tok, value) if err then return nil, context, err end
        end,
    }
    local value
    local err
    value, global_context, err = visit[ast.type](visit, ast, global_context)
    return value, global_context, err
end
---Execution
local fn
local text
if arg[1] then text = string.join("\n", lines_from(arg[1])); fn = arg[1] else text = string.join("\n", lines_from(arg[0]:sub(1, #arg[0]-8).."test.o")); fn = "test.o" end
local tokens, err = lex(fn, text) if err then print(err.repr(err, text)) return end if tokens[1].type == T.eof then return end
--print(string.join("-", tokens))
local ast
ast, err = parse(tokens) if err then print(err.repr(err, text)) return end
--print(ast)
local value, global_context = nil, Context({
    pi = { value = Number(math.pi), kw = "const" },
})
value, global_context, err = interpret(ast, global_context)
if err then print(err.repr(err, text)) else if value then if value.str then print(value.str(value)) end end end