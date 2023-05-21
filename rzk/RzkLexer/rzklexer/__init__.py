import pygments.lexer
from pygments.lexer import bygroups
from pygments.token import *
__all__ = ["RzkLexer"]
class RzkLexer(pygments.lexer.RegexLexer):
    name = 'Rzk'
    aliases = ['rzk']
    filenames = ['*.rzk']
    url = 'https://github.com/fizruk/rzk'
    KEYWORDS = ['as', 'uses']
    def get_tokens_unprocessed(self, text):
        for index, token, value in super(RzkLexer,self).get_tokens_unprocessed(text):
            if token is Name and value in self.KEYWORDS:
                yield index, Keyword, value
            else:
                yield index, token, value
    tokens = {
        'root': [
            (r'--.*\n', Comment),
            (r'\{-((.)(?<!-))*-((.)(?<![-\}])((.)(?<!-))*-|-)*\}', Comment),
            (r'rzk-1', String),
            (r'U|CUBE|TOPE|1|2|Sigma|∑|Σ', Keyword.Type),
            (r'\*_1|0_2|1_2|refl|BOT|recBOT|TOP|first|second|idJ|recOR', Name.Constant),
            (r'#lang|#set-option|#unset-option|#check|#compute-whnf|#compute-nf|#compute', Name.Decorator),
            (r'(#section|#end)(\s+[^\t\n\r !"#\(\),-\.;:<>\?\[\\\]\{\|\}][^\t\n\r !"#\(\),\.;:<>\?\[\\\]\{\|\}]*)',
                bygroups(Name.Decorator, Name.Entity)),
            (r'(\(\s+)([^\t\n\r !"#\(\),-\.;:<>\?\[\\\]\{\|\}][^!"#\(\),\.;:<>\?\[\\\]\{\|\}]*)(:)',
                bygroups(Punctuation, Name.Variable, Punctuation)),
            (r'(\\)(([^\t\n\r !"#\(\),-\.;:<>\?\[\\\]\{\|\}][^\t\n\r !"#\(\),\.;:<>\?\[\\\]\{\|\}]*)\s*)*',
                bygroups(Punctuation, Name.Variable)),
            (r';|:|:=|\(|\)|_|,|\{|\||\}|\|\[|\]|<|>|\\|->', Punctuation),
            (r' = | \* | === | <= | /\\ | \\/ ', Operator),
            (r'((#assume|#variables|#variable)\s+)([^:]+)',
                bygroups(Keyword.Declaration, None, Name.Variable)),
            (r'((#postulate|#define|#def)\s+)([^\t\n\r !"#\(\),-\.;:<>\?\[\\\]\{\|\}][^\t\n\r !"#\(\),\.;:<>\?\[\\\]\{\|\}]*\s+)((uses\s+)(\()([^\(\)]+)(\)))?',
                bygroups(Keyword, None, Name.Function, None, Keyword, Punctuation, Name.Variable, Punctuation)),
            (r'"((.)(?<!["\\])|\\["\\nt])*"', String.Double),
            (r'\s+', Token.Space),
            (r'(.)(?<![\t\n\r !"#\(\),-\.;:<>\?\[\\\]\{\|\}])((.)(?<![\t\n\r "#\(\),;<>\[\\\]\{\|\}]))*', Name),
            (r'\?', Name),
            (r'[a-zA-Z]([a-zA-Z]|\d|_|\')*', Name)
        ]
    }
