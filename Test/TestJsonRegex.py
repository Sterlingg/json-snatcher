import unittest
import re
import tokenizer

class TestJson(unittest.TestCase):
    
    def setUp(self):
        pass

    def test_rfc_object(self):
        expected = ['{', 
                    '"Image"', ':', '{', 
                    '"Width"', ':', '800', ',', 
                    '"Height"', ':', '600', ',',
                    '"Title"', ':', '"View from 15th Floor"', ',',
                    '"Thumbnail"', ':', '{',
                    '"Url"', ':', '"http://www.example.com/image/481989943"', ',',
                    '"Height"', ':' , '125', ',',
                    '"Width"', ':', '"100"',
                    '}', ',',
                    '"IDs"', ':', '[', '116', ',', '943', ',',
                    '234', ',', '38793', ']',
                    '}',
                    '}',
                    ]

        with open('rfc_object.json','rb') as f:
            test_str = f.read()
            test_str = r'' + test_str
            tkenizer = tokenizer.Tokenizer(test_str)
            result = tkenizer.tokenize()
            i = 0
            for token in result:
                self.assertEqual(token, expected[i])
                i += 1

    def test_rfc_array(self):
        expected = [ 
      '[',
      '{',
         '"precision"', ':', '"zip"', ',',
         '"Latitude"', ':', '37.7668', ',',
         '"Longitude"', ':', '-122.3959', ',',
         '"Address"', ':', '""' , ',',
         '"City"', ':', '"SAN FRANCISCO"', ',',
         '"State"', ':', '"CA"', ',',
         '"Zip"', ':', '"94107"', ',',
         '"Country"', ':', '"US"',
       '}', ',',
      '{',
         '"precision"', ':', '"zip"', ',',
         '"Latitude"', ':', '37.371991', ',',
         '"Longitude"', ':', '-122.026020', ',',
         '"Address"', ':', '""', ',',
         '"City"', ':', '"SUNNYVALE"', ',',
         '"State"', ':', '"CA"', ',',
         '"Zip"', ':', '"94085"', ',',
         '"Country"', ':', '"US"',
       '}',
       ']',
        ]

        with open('rfc_array.json','rb') as f:
            test_str = f.read()
            test_str = r'' + test_str


            tkenizer = tokenizer.Tokenizer(test_str)
            result = tkenizer.tokenize()
            i = 0
            for token in result:
                self.assertEqual(token, expected[i])
                i += 1

if __name__ == '__main__':
    unittest.main()
