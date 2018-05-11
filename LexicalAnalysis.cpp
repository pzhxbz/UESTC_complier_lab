#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <map>

using namespace std;

inline void ERROR_MSG(const char *msg)
{
    cerr << msg << endl;
    exit(-1);
}

class Analysis
{
  public:
    enum WordType
    {
        BEGIN = 1,
        END = 2,
        INTEGER = 3,
        IF = 4,
        THEN = 5,
        ELSE = 6,
        FUNCTION = 7,
        READ = 8,
        WRITE = 9,
        SYMBOL = 10,
        CONSTANT = 11,
        EQ = 12,
        NE = 13,
        LE = 14,
        LT = 15,
        BE = 16,
        BT = 17,
        SUB = 18,
        MUL = 19,
        ASSIGN = 20,
        LBRAC = 21,
        RBRAC = 22,
        SEM = 23
        //BUFLEN = 50
    };
    enum ErrorTypr
    {
        OPERA_ERROR = 1,
        SYM_ERROR = 2
    };
    Analysis()
    {
        //symbol = new map<string, int>();
        initTable();
    }
    Analysis(string source)
    {
        this->source = source;
        initTable();
    }
    ~Analysis()
    {
    }

    void StartAnalysis()
    {
        auto p = this->source.begin();
        string tmp_sym = "";
        while (p != this->source.end())
        {
            if (*p == ' ' || *p == '\n')
            {
                if (tmp_sym.length() == 0)
                {
                    p++;
                    continue;
                }
                if (IsLetterAndNumber(tmp_sym.back()))
                    goto ERROR_SY;
                else
                    goto ERROR_OP;
            }
            if (tmp_sym.length() != 0)
            {
                if (IsLetterAndNumber(*p) && (!IsLetterAndNumber(tmp_sym.back())))
                {
                ERROR_OP:
                    MatchOp(tmp_sym);
                    tmp_sym.clear();
                    goto END;
                    // if(*p!=' '&&*p!='\n')
                    //     tmp_sym += *p;
                    // p++;
                    // continue;
                }
                if (IsOperator(*p) && (!IsOperator(tmp_sym.back())))
                {
                ERROR_SY:
                    MatchSym(tmp_sym);
                    tmp_sym.clear();
                    goto END;
                   
                }
            }
        END:
            if (*p != ' ' && *p != '\n')
                tmp_sym += *p;
            else if (*p == '\n')
            {
                line++;
                WriteFile("NEXTLINE", 66);
            }
            p++;
        }
        if (!IsLetterAndNumber(tmp_sym.back()))
        {
            MatchOp(tmp_sym);
        }
        else
        {
            MatchSym(tmp_sym);
        }
    }

    //template <typename OF>
    void DumpOutput(ostream &stream)
    {
        stream << output << endl;
    }

    //template <typename OF>
    void DumpError(ostream &stream)
    {
        stream << error << endl;
    }

    void SetSource(string source)
    {
        this->source = source;
    }

  private:
    void MatchOp(string tmp_sym)
    {
        if (!MatchSymbol(tmp_sym))
        {
            for (auto f = tmp_sym.begin(); f != tmp_sym.end();)
            {
                if (isSingleOpera(*f))
                {
                    string tmp = string("") + *f;
                    if (!MatchSymbol(tmp))
                    {
                        WriteError(tmp, OPERA_ERROR);
                    }
                    f++;
                }
                else
                {
                    string tmp = string("") + *f;
                    f++;
                    if (f == tmp_sym.end())
                    {
                        WriteError(tmp, OPERA_ERROR);
                        break;
                    }
                    tmp += *f;
                    if (!MatchSymbol(tmp))
                    {
                        WriteError(tmp, OPERA_ERROR);
                    }
                    f++;
                }
            }
        }
    }

    void MatchSym(string tmp_sym)
    {
        if (!MatchSymbol(tmp_sym))
        {
            if (IsAllNumber(tmp_sym))
            {
                WriteFile(tmp_sym, CONSTANT);
            }
            else
            {
                if (CheckSym(tmp_sym))
                {
                    WriteFile(tmp_sym, SYMBOL);
                }
                else
                {
                    WriteError(tmp_sym, SYM_ERROR);
                }
            }
        }
    }

    bool IsLetterAndNumber(char a)
    {
        return IsLetter(a) | IsNumber(a);
    }

    bool IsLetter(char a)
    {
        if (a >= 'a' && a <= 'z')
        {
            return true;
        }
        if (a >= 'A' && a <= 'Z')
        {
            return true;
        }
        return false;
    }
    bool IsNumber(char a)
    {
        if (a >= '0' && a <= '9')
        {
            return true;
        }
        return false;
    }

    bool IsAllNumber(string a)
    {

        for (auto p = a.begin(); p != a.end(); p++)
        {
            if (!IsNumber(*p))
            {
                return false;
            }
        }
        return true;
    }

    bool CheckSym(string sym)
    {
        if (sym.length() == 0)
        {
            return true;
        }
        if (IsNumber(sym[0]))
        {
            return false;
        }
        return true;
    }

    bool MatchSymbol(string sym)
    {
        if (sym.length() == 0)
        {
            return false;
        }
        //auto finded = symbol.find(sym);
        if (symbol.count(sym))
        {
            //auto p = *finded;
            WriteFile(sym, symbol[sym]);
            return true;
        }
        return false;
    }

    bool IsOperator(char a)
    {
        if (operatorTable.find(a) != operatorTable.npos)
        {
            return true;
        }
        return false;
    }

    bool isSingleOpera(char a)
    {
        if (singleOpera.find(a) != singleOpera.npos)
        {
            return true;
        }
        return false;
    }

    void WriteFile(string sym, int type)
    {
        output += sym + '\t' + to_string(type) + '\n';
    }

    void WriteError(string err_sym, int type)
    {
        if (type == OPERA_ERROR)
        {
            error += "LINE:行号" + to_string(line + 1) + "  运算符" + err_sym + "不正确\n";
        }
        if (type == SYM_ERROR)
        {
            error += "LINE:行号" + to_string(line + 1) + "  标识符" + err_sym + "不正确\n";
        }
    }

    void initTable()
    {
        symbol.insert(pair<string, int>("begin", 1));
        symbol.insert(pair<string, int>("end", 2));
        symbol.insert(pair<string, int>("integer", 3));
        symbol.insert(pair<string, int>("if", 4));
        symbol.insert(pair<string, int>("then", 5));
        symbol.insert(pair<string, int>("else", 6));
        symbol.insert(pair<string, int>("function", 7));
        symbol.insert(pair<string, int>("read", 8));
        symbol.insert(pair<string, int>("write", 9));
        symbol.insert(pair<string, int>("symbol", 10));
        symbol.insert(pair<string, int>("constant", 11));
        symbol.insert(pair<string, int>("=", 12));  // eq
        symbol.insert(pair<string, int>("<>", 13)); // ne
        symbol.insert(pair<string, int>("<=", 14));
        symbol.insert(pair<string, int>("<", 15));
        symbol.insert(pair<string, int>(">=", 16));
        symbol.insert(pair<string, int>(">", 17));
        symbol.insert(pair<string, int>("-", 18));
        symbol.insert(pair<string, int>("*", 19));
        symbol.insert(pair<string, int>(":=", 20));
        symbol.insert(pair<string, int>("(", 21));
        symbol.insert(pair<string, int>(")", 22));
        symbol.insert(pair<string, int>(";", 23));
        symbol.insert(pair<string, int>("NEXTLINE", 50));
    }

    map<string, int> symbol;

    string source;
    string output;
    string error;
    string operatorTable = "%&+=-*<>^\\:();";
    string singleOpera = "*();-";

    int line = 0;
};

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        cout << "use LexicalAnalysis <input_file>" << endl;
    }
    ifstream source(argv[1]);
    if (!source.is_open())
    {
        ERROR_MSG("falied to open");
    }
    ofstream out("out_test.dyd");
    if (!out.is_open())
    {
        ERROR_MSG("failed to open out");
    }

    ofstream error("error_test.err");

    if (!error.is_open())
    {
        ERROR_MSG("open error file failed");
    }
    string data{istreambuf_iterator<char>{source}, istreambuf_iterator<char>{}};

    source.close();

    Analysis analysiser{data};

    analysiser.StartAnalysis();

    analysiser.DumpOutput(out);
    analysiser.DumpError(error);

    return 0;
}
