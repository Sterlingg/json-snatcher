import unittest
import re

class TestStringRegex(unittest.TestCase):
    
    def setUp(self):
        #"\\/bfnrt
        escaped_regex = r'(?:(?:\\\\)*\\(?:(?:[\S]|(?:u[0-9A-Fa-f]{4}))))'
      #  escaped_regex = r'(?:(?:\\)*\\")'
        unescaped_regex = r'(?:[^"\\\s])*(?:\s)*'
        self.regex = re.compile('"(?:' + unescaped_regex + '|' + escaped_regex + ')+"',re.DEBUG)
        print '"(?:' + unescaped_regex + '|' + escaped_regex + ')+"'

    def test_simple(self):
        test_str = r'"abcde 12345 6789 10"'
        result = self.regex.findall(test_str)
        self.assertEqual(r'"abcde 12345 6789 10"', result[0])
        # self.assertTrue(element in self.seq)

    def test_multiple_strings(self):
        test_str = r'"abcde" "12345" "6789" "10"'
        result = self.regex.findall(test_str)
        self.assertEqual(r'"abcde"', result[0])
        self.assertEqual(r'"12345"', result[1])
        self.assertEqual(r'"6789"', result[2])
        self.assertEqual(r'"10"', result[3])

    def test_strings_w_quote_escapes(self):
        test_str = r'"abcde\"" "12345\\" "sterling\\\\" "\\6789\\\" \"10\uabcd2\q0" "\\\\"""'
        result = self.regex.findall(test_str)
        self.assertEqual(r'"abcde\""', result[0])
        self.assertEqual(r'"12345\\"', result[1])
        self.assertEqual(r'"sterling\\\\"', result[2])
        self.assertEqual(r'"\\6789\\\" \"10\uabcd2\q0"', result[3])
        self.assertEqual(r'"\\\\"', result[4])
        self.assertEqual(r'""', result[5])

    def test_reg_escapes(self):
        escapes = [r'\"', r'\\', r'\/', r'\b', r'\f', r'\n', r'\r', r'\t', r'\uAa23']

        for escape in escapes:
            test_str = r'"abcde 12345 ' + escape + '6789 10"'
            result = self.regex.findall(test_str)
            self.assertEqual(r'"abcde 12345 ' + escape + '6789 10"', result[0])

    def test_escapes_dbl(self):
        escapes = [r'\"', r'\\', r'\/', r'\b', r'\f', r'\n', r'\r', r'\t', r'\uAa23']

        for escape in escapes:
            test_str = r'"abcde 12345 \\' + escape + r'6789 10"'
            result = self.regex.findall(test_str)
            self.assertEqual(r'"abcde 12345 \\' + escape + r'6789 10"', result[0])

    def test_mt_string(self):
        test_str = r'""'
        result = self.regex.findall(test_str)
        self.assertEqual(r'""', result[0])

    def test_url(self):
        test_str = r'"http://www.example.com/image/481989943"'
        result = self.regex.findall(test_str)
        self.assertEqual(r'"http://www.example.com/image/481989943"', result[0])
        
    def test_rfc_object(self):

        expected = [
            '"Image"',
            '"Width"',
            '"Height"',
            '"Title"', '"View from 15th Floor"',
            '"Thumbnail"',
            '"Url"','"http://www.example.com/image/481989943"',
            '"Height"',
            '"Width"', '"100"',
            '"IDs"']

        with open('rfc_object.json','rb') as f:
            test_str = f.read()
            test_str = r'' + test_str

            result = self.regex.findall(test_str)
            i = 0
            for r in result:
                self.assertEqual(r, expected[i])
                i += 1

    def test_rfc_array(self):
        expected = [
         '"precision"', '"zip"',
         '"Latitude"',
         '"Longitude"',
         '"Address"', '""',
         '"City"', '"SAN FRANCISCO"',
         '"State"', '"CA"',
         '"Zip"', '"94107"',
         '"Country"', '"US"',
         '"precision"', '"zip"',
         '"Latitude"',
         '"Longitude"',
         '"Address"', '""',
         '"City"', '"SUNNYVALE"',
         '"State"', '"CA"',
         '"Zip"', '"94085"',
         '"Country"', '"US"']

        with open('rfc_array.json','rb') as f:
            test_str = f.read()
            test_str = r'' + test_str

            result = self.regex.findall(test_str)
            i = 0
            for r in result:
                self.assertEqual(r, expected[i])
                i += 1

    def test_long_json(self):
        with open('aws.json','rb') as f:
            test_str = f.read()
            test_str = r'' + test_str
            result = self.regex.findall(test_str)
            self.assertEqual(result[0],'"config"')
            self.assertEqual(result[10],'"us-east"')
            self.assertEqual(result[len(result)-1], '"vers"')

if __name__ == '__main__':
    unittest.main()



