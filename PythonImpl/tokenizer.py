import re

class Tokenizer():
    """
    Class to tokenize the JSON data. Parses into 4 classes of tokens strings,
    numbers, values, and delimeters.

    Defined as per RFC 4627.
    """
    
    def __init__(self, json):
        """
        Arguments:
        - `json`: The JSON data being tokenized.
        """
        self._json = json
        
    def tokenize(self):
        """
        Breaks the JSON string into tokens to be used by the parser.
        """
        tokens = []

        number_regex = r'-?(?:0|(?:[1-9][\d]*))(?:\.\d+)?(?:[eE][-+]?[\d]+)?'
        string_regex = r'"(?:(?:[^"\\\s])*(?:\s)*|(?:(?:\\\\)*\\(?:(?:[\S]|(?:u[0-9A-Fa-f]{4})))))+"'
        delimeters = '[[\]{}:,]'
        json_regex = re.compile('(' + number_regex + ')' 
                                + '|' + '(' + string_regex + ')'
                                + '|' + '(' + delimeters + ')')

        result = json_regex.findall(self._json)

        for group in result:
            for token in group:
                if token != "":
                    tokens.append(token)

        return tokens
