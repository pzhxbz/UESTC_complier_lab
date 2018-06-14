#include <iostream>
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <map>
#include <list>

using namespace std;

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
    SEM = 23,
    EOLN = 24,
    MYEOF = 25
};

inline void ERROR_MSG(const char *msg)
{
    cerr << msg << endl;
    exit(-1);
}

class Variable
{
  public:
    Variable(string name, string process, int tpName, int position) : name(name), process(process), tpName(tpName), position(position) {}

    string GetName()
    {
        return name;
    }
    int GetType()
    {
        return tpName;
    }
    string GetProcess()
    {
        return process;
    }

    int GetPosition()
    {
        return position;
    }

    string Format(int level, string padding)
    {
        string res = padding + "Variable:\n";
        res += padding + "name : " + name + "\n";
        res += padding + "proc : " + process + "\n";
        res += padding + "kind : 0\n";
        if (tpName == FUNCTION)
        {
            res += padding + "type : function\n";
        }
        else if (tpName == INTEGER)
        {
            res += padding + "type : integer\n";
        }
        res += padding + "vlev : " + to_string(level) + "\n";
        res += padding + "vadr : " + to_string(position) + "\n";
        return res;
    }

  private:
    string name;
    string process;
    int tpName; // type name
    int position;
};

class Process
{
  public:
    Process(string name, int level) : processName(name), level(level) {}
    Process()
    {
    }
    void AddVar(Variable a)
    {
        vars.push_back(a);
    }
    bool HashVar(Variable a)
    {
        //return true;
        for (auto p = vars.begin(); p != vars.end(); p++)
        {
            if (p->GetName() == a.GetName() ) //&& a.GetType() == p->GetType()
            {
                return true;
            }
        }
        return false;
    }
    list<Variable> GetVar()
    {
        return this->vars;
    }
    string GetName()
    {
        return processName;
    }

    int getLevel()
    {
        return this->level;
    }

    string Format(string padding)
    {
        if (GetName() == "main")
        {
            return "";
        }
        string res = padding + "Process";
        res += padding + "name : " + GetName() + "\n";
        res += padding + "type : function\n";
        res += padding + "plev : " + to_string(level) + "\n";
        res += padding + "fasr : " + to_string(vars.front().GetPosition()) + "\n";
        res += padding + "ladr : " + to_string(vars.back().GetPosition()) + "\n";
        return res;
    }

  private:
    list<Variable> vars;
    string processName;
    int level;
};

struct SymInfo
{
    string name;
    int type;
};

class Analysis
{
  public:
    Analysis(string data)
    {
        this->source = data;
        string name;
        int t;
        istringstream input(data);
        while (input >> name >> t)
        {
            sourceList.push_back(SymInfo{name, t});
        }
    }
    ~Analysis()
    {
        for (auto p = allProcess.begin(); p != allProcess.end(); p++)
        {
            delete (*p);
        }
    }
    void StartAnalysis()
    {
        auto p = sourceList.begin();
        while (p->type == EOLN)
        {
            p++;
            line++;
        }
        nowProcess = new Process("main", 0);
        allProcess.push_back(nowProcess);
        auto next = S(p);
        if (next->type != MYEOF)
        {
            WriteError("can find eof");
        }
    }

    void dumpError(ostream &stream)
    {
        stream << error << endl;
    }

    void dumpVar(ostream &stream)
    {
        for (auto p = allProcess.begin(); p != allProcess.end(); p++)
        {
            string padding = "";
            for (int i = 0; i < (*p)->getLevel(); i++)
            {
                padding += "\t";
            }
            stream << (*p)->Format(padding);
            auto vars = (*p)->GetVar();
            for (auto v = vars.begin(); v != vars.end(); v++)
            {
                stream << v->Format((*p)->getLevel(), padding);
            }
        }
    }

  private:
    void addVar(string name, int type, int NotNext = 0)
    {

        Variable tmpVar = Variable(name, nowProcess->GetName(), type, ++varCount-NotNext);
        varCount -= NotNext;
        if (nowProcess->HashVar(tmpVar))
        {
            WriteError("muti define var_name " + name);
        }
        else
        {
            nowProcess->AddVar(tmpVar);
        }
        //varTable.push_back(Variable{name, process, type});
    }

    list<SymInfo>::iterator S(list<SymInfo>::iterator p)
    {
        auto next = _get_next(p);
        if (p->type != BEGIN)
        {
            //_get_last(p);
            WriteError("can't find begin,find " + p->name);
            //next =
        }

        next = A(next);
        auto nnext = _get_next(next);
        if (next->type != SEM)
        {
            WriteError("missing ;");
            nnext = _get_last(nnext);
            //line--;
        }
        next = B(nnext);
        if (next->type != END)
        {
            WriteError("can't find end");
            //next = B(_get_next(next));
            //next = _get_last(next);
        }
        return _get_next(next);
    }
    list<SymInfo>::iterator A(list<SymInfo>::iterator p)
    {
        return AS(C(p));
    }
    list<SymInfo>::iterator AS(list<SymInfo>::iterator p)
    {
        if (p->type != SEM)
        {
            return p;
        }
        auto p_next = _get_next(p);
        auto next = C(p_next);
        if (next == p_next)
        {
            return _get_last(p_next);
        }
        return AS(next);
    }
    list<SymInfo>::iterator B(list<SymInfo>::iterator p)
    {
        return BS(Z(p));
    }
    list<SymInfo>::iterator BS(list<SymInfo>::iterator p)
    {
        if (p->type != SEM)
        {
            return p;
        }
        p = _get_next(p);
        auto next = Z(p);
        if (next != p)
        {
            next = BS(next);
            return next;
        }
        return p;
    }

    list<SymInfo>::iterator C(list<SymInfo>::iterator p) //
    {
        if (p->type == INTEGER)
        {
            return H(_get_next(p));
        }
        else
        {
            auto backup = p;
            p = _get_next(p);
            auto next = H(p);
            if (next == p)
            {
                return _get_last(p);
            }
            if (next->type == SEM)
            {
                WriteError("can't find integer, find " + backup->name);
                return next;
            }
            return _get_last(p);
        }
        return p;
    }
    list<SymInfo>::iterator H(list<SymInfo>::iterator p)
    {
        auto next = I(p);
        if (next == p)
        {
            if (p->type != FUNCTION)
            {
                return p;
                WriteError("can't find function");
                p = _get_last(p);
            }
            p = _get_next(p);
            next = I(p);
            addVar(p->name, FUNCTION);

            auto nnext = _get_next(next);
            if (next->type != LBRAC)
            {
                WriteError("missing (");
                nnext = _get_last(nnext);
            }
            next = I(nnext);
            if (next->type != RBRAC)
            {
                WriteError("missing )");
                next = _get_last(next);
            }
            nnext = _get_next(next);
            if (nnext->type != SEM)
            {
                WriteError("missing ;");
                nnext = _get_last(nnext);
            }
            lastProcess = nowProcess;
            nowProcess = new Process(p->name, lastProcess->getLevel() + 1);
            allProcess.push_back(nowProcess);
            addVar(p->name, FUNCTION, 1);
            auto ret = S(_get_next(nnext));
            nowProcess = lastProcess;
            return ret;
        }
        if(next->type == SEM)
        {
            addVar(p->name, INTEGER);
        }
        
        return next;
    }
    list<SymInfo>::iterator I(list<SymInfo>::iterator p)
    {
        if (p->type == SYMBOL)
        {
            return _get_next(p);
        }
        return p;
        //return IS(C(p));
    }

    list<SymInfo>::iterator IS(list<SymInfo>::iterator p)
    {
        auto next = p;
        return IS(next);
    }

    list<SymInfo>::iterator J(list<SymInfo>::iterator p)
    {
        return JS(X(p));
    }

    list<SymInfo>::iterator JS(list<SymInfo>::iterator p)
    {
        //auto next = PP(p);
        if (p->type == SUB)
        {
            auto next = _get_next(p);
            return JS(X(next));
        }
        return p;
    }

    list<SymInfo>::iterator X(list<SymInfo>::iterator p)
    {
        return XS(Y(p));
    }

    list<SymInfo>::iterator XS(list<SymInfo>::iterator p)
    {
        auto next = Y(p);
        if (next == p)
        {
            if (next->type != MUL)
            {
                //WriteError("missing operator");
                //next = _get_last(next);
                return next;
            }
            next = Y(_get_next(p));
            if (next == p)
            {
                return p;
            }
            return XS(next);
        }
        return p;
    }

    list<SymInfo>::iterator P(list<SymInfo>::iterator p)
    {
        if (p->type <= BT && p->type >= EQ)
        {
            // success op
            return _get_next(p);
        }
        return p;
    }

    list<SymInfo>::iterator K(list<SymInfo>::iterator p)
    {
        return J(P(J(p)));
    }

    list<SymInfo>::iterator Y(list<SymInfo>::iterator p)
    {
        auto next = I(p);
        if (p != next || p->type == LBRAC)
        {
            if (next->type == LBRAC)
            {
                // function call
                next = _get_next(next);
                CheckVar(p->name, FUNCTION);
                next = J(next);
                if (next->type != RBRAC)
                {
                    WriteError("missing )");
                    next = _get_last(next);
                }
                return _get_next(next);
            }
            CheckVar(p->name, INTEGER);
            return next;
        }
        if (next->type == CONSTANT)
        {
            return _get_next(p);
        }

        // function call missing here

        return p;
    }

    list<SymInfo>::iterator Z(list<SymInfo>::iterator p)
    {
        if (p->type == READ)
        {
            p = _get_next(p);
            auto nnext = _get_next(p);
            if (p->type != LBRAC)
            {
                nnext = _get_last(nnext);
                WriteError("missing (");
            }
            auto next = I(nnext);
            CheckVar(nnext->name, INTEGER);
            nnext = _get_next(next);
            if (next->type != RBRAC)
            {
                nnext = _get_last(nnext);
                WriteError("missing )");
            }
            return nnext;
        }
        if (p->type == WRITE)
        {
            p = _get_next(p);
            if (p->type != LBRAC)
            {
                p = _get_last(p);
                WriteError("missing (");
            }
            auto next = I(_get_next(p));
            CheckVar(p->name, INTEGER);
            if (next->type != RBRAC)
            {
                next = _get_last(next);
                WriteError("missing )");
            }
            return _get_next(next);
        }
        auto next = I(p);
        if (next != p)
        {
            CheckVar(p->name, INTEGER);
            if (next->type != ASSIGN)
            {

                next = _get_last(next);
                //WriteError(string("can't ") + next->name);
            }
            return J(_get_next(next));  // function
        }
        if (p->type == IF)
        {
            //next = K(_get_next(p));
            p = _get_next(p);
            //if<条件表达式>then<执行语句>else <执行语句>
            next = K(p);

            if (next->type != THEN)
            {
                //next = _get_next(next);
                WriteError("missing then,find " + next->name);
            }
            next = Z(_get_next(next));

            if (next->type != ELSE)
            {

                //next = _get_last(next);
                WriteError("can't find else,find "+next->name);
            }
            next = Z(_get_next(next));
            return next;
        }
        return p;
    }

    list<SymInfo>::iterator _get_next(list<SymInfo>::iterator p)
    {
        p++;
        while (p->type == EOLN)
        {
            p++;
            line++;
        }
        return p;
    }
    list<SymInfo>::iterator _get_last(list<SymInfo>::iterator p)
    {
        p--;
        while (p->type == EOLN)
        {
            p--;
            line--;
        }
        return p;
    }

    void WriteFile(string msg)
    {
    }

    void WriteError(string msg)
    {
        error += "LINE:行号" + to_string(line) + "    " + msg + "\n";
    }

    void CheckVar(string varName, int type)
    {
        if (!IsLetterAndNumber(varName[0]))
        {
            return;
        }
        if (nowProcess->HashVar(Variable(varName, nowProcess->GetName(), type, 0)))
        {
            return;
        }
        WriteError("undefined var " + varName);
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
    Process *nowProcess;
    Process *lastProcess;
    list<Process *> allProcess;
    list<SymInfo> sourceList;
    string error;
    string sym;
    string source;
    int line = 1;
    int varCount = 0;
    //list<SymInfo> lines;
};

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        cout << "use SyntacticAnalysis <dyd_file>" << endl;
        exit(0);
    }
    ifstream source(argv[1]);
    if (!source.is_open())
    {
        ERROR_MSG("falied to open");
    }
    ofstream out("out.dys");
    if (!out.is_open())
    {
        ERROR_MSG("failed to open out");
    }

    ofstream tables("tables");
    if (!tables.is_open())
    {
        ERROR_MSG("failed to open tables");
    }

    ofstream error("serror.err");

    if (!error.is_open())
    {
        ERROR_MSG("open error file failed");
    }
    string data{istreambuf_iterator<char>{source}, istreambuf_iterator<char>{}};

    source.close();
    out << data;
    out.close();
    Analysis anlysisor = Analysis(data);
    anlysisor.StartAnalysis();

    anlysisor.dumpError(error);
    anlysisor.dumpVar(tables);

    tables.close();
    error.close();

    return 0;
}