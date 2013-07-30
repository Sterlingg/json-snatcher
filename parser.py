import tokenizer

class ParseError(Exception):
    """
    Exception raised if a particular token isn't encountered when it is expected.
    """
    
    def __init__(self, value):
        """
        """
        self.value = value
    def __str__(self):
        """
        """
        return repr(self.value)
        

class JSONArray(object):
    """
    """
    
    def __init__(self):
        """
        """
        self.members = []
        

class JSONObject(object):
    """
    """
    
    def __init__(self):
        """
        """
        self.members = {}

class Literal(object):
    """
    """
    
    def __init__(self, literal):
        """
        Arguments:
        - `literal`:
        """
        self.literal = literal

class Member(object):
    """
    """
    
    def __init__(self, key, value):
        """
        """
        self.key = key
        self.value = value

class Number(object):
    """
    """
    
    def __init__(self, number):
        """
        """
        self.number = number
 
class String(object):
    """
    """
    
    def __init__(self, string):
        """
        """
        self.string = string

class Value(object):
    """
    """
    
    def __init__(self, value):
        """
        """
        self.value = value

class Parser(object):
    """
    """
    
    def __init__(self, json):
        """
        """

        self.tokens = tokenizer.Tokenizer(json).tokenize()
        self.curr_token = 0
        
    def consume_token(self):
        """
        Advances the parser to the next token. 
        
        Returns -1 when there are no tokens left.
        """

        if self.curr_token <= len(self.tokens):
            self.curr_token += 1
            return self.tokens[self.curr_token - 1]
        else:
            return None
        
    def json_object(self):
        """
        object = { member (, member)* }
        member = string  : value
        value = object/array/number/string/literal
        literal = false/null/true
        """
        token = self.consume_token()
        obj = JSONObject()

        while token != '}':
            if token == ',':
                token = self.consume_token()
            else:
                member = self.member(token)
                obj.members[member.key] = member.value
                token = self.consume_token()
        
        return obj

    def member(self, token):
        """
        """
        key = token

        if key[0] != '"':
            raise ParseError('Expected string in member. Got ' + key + self.tokens[self.curr_token-1])

        if self.consume_token() != ':':
            raise ParseError('Expected : in member. Got ' + key)

        value = self.value(self.consume_token())
            
        return Member(key, value)

    def value(self, token):
        """
        """
        
        if token == '{':
            value = self.json_object()
        elif token == '[':
            value = self.json_array()
        elif token[0] == '"':
            value = String(token)
        elif token[0].isdigit():
            value = Number(token)
        else:
            value = Literal(token)

        return Value(value)
        
    def json_array(self):
        """
        """
        token = self.consume_token()
        arr = JSONArray()

        while token != ']':
            if token == ',':
                token = self.consume_token()
            else:
                value = self.value(token)
                arr.members.append(value)
                token = self.consume_token()
        
        return arr

    def parse(self):
        """
        """
        token = self.consume_token()
        
        if token == '{':
            result = self.json_object()
        elif token == '[':
            result = self.json_array()
        else:
            result = None

        return result
                
if __name__ == '__main__':
    with open('Test/rfc_array.json','rb') as f:
        test_str = f.read()
        parser = Parser(test_str)

    print parser.parse()
