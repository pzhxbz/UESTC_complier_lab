#include <iostream>
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <map>
#include <list>

using namespace std;

inline void ERROR_MSG(const char *msg)
{
    cerr << msg << endl;
    exit(-1);
}

class Variable
{
  public:
    Variable(string name, string process, int tpName) : name(name), process(process), tpName(tpName) {}

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

  private:
    string name;
    string process;
    int tpName; // type name
};

class Process
{
  public:
  private:
    string processName;
};

struct SymInfo
{
    string name;
    int type;
};

class Analysis
{
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
        NEXTLINE = 66
    };

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
    void StartAnalysis()
    {
        auto p = sourceList.begin();
        while(p->type == NEXTLINE)
        {
            p++;
            line++;
        }
        S(p);
    }

    void dumpError(ostream &stream)
    {
        stream<<error<<endl;
    }
  private:
    void addVar(string name, string process, int type)
    {
        varTable.push_back(Variable{name, process, type});
    }

    list<SymInfo>::iterator S(list<SymInfo>::iterator p)
    {
        auto next = _get_next(p);
        if (p->type != BEGIN)
        {
            WriteError("can't find begin");
            next = _get_last(p);
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
            next = _get_last(next);
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
        if(next == p_next)
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
        if(p->type != SEM)
        {
            return p;
        }
        p = _get_next(p);
        auto next = Z(p);
        if(next != p)
        {
            next = BS(next);
            return next;
        }
        return p;
    }

    list<SymInfo>::iterator C(list<SymInfo>::iterator p)  // 
    {
        if (p->type == INTEGER)
        {
            return H(_get_next(p));
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
                WriteError("can't find function");
                p = _get_last(p);
            }
            next = I(_get_next(p));
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
            return S(_get_next(nnext));
        }
        return next;
    }
    list<SymInfo>::iterator I(list<SymInfo>::iterator p)
    {
        if(p->type == SYMBOL)
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
        if(p->type == SUB)
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
        if(next==p)
        {
            if(next->type != MUL)
            {
                //WriteError("missing operator");
                //next = _get_last(next);
                return next;
            }
            next = Y(_get_next(p));
            if(next == p)
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
        if(p != next)
        {
            if(next->type == LBRAC)
            {
                // function call
                next = J(_get_next(next));
                if(next->type != RBRAC)
                {
                    WriteError("missing )");
                    next = _get_last(next);
                }
                return _get_next(next);

            }
            return next;
        }
        if(next->type == CONSTANT)
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
            auto nnext =_get_next(p);
            if (p->type != LBRAC)
            {
                nnext = _get_last(nnext);
                WriteError("missing (");
            }
            auto next = I(nnext);
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
            if (next->type != ASSIGN)
            {
                
                next = _get_last(next);
                WriteError(string("can't ") + next->name);
            }
            return J(_get_next(next));
        }
        if (p->type == IF)
        {
            //next = K(_get_next(p));
            p = _get_next(p);
            //if<条件表达式>then<执行语句>else <执行语句>
            next = K(p);

            if(next->type != THEN)
            {
                next = _get_last(next);
                WriteError("missing then");
            }
            next = Z(_get_next(next));

            if(next->type != ELSE)
            {
                
                next = _get_last(next);
                WriteError("can't find else");
            }
            next = Z(_get_next(next));
            return next;
        }
        return p;
    }

    list<SymInfo>::iterator _get_next(list<SymInfo>::iterator p)
    {
        p++;
        while(p->type == NEXTLINE)
        {
            p++;
            line++;
        }
        return p;
    }
    list<SymInfo>::iterator _get_last(list<SymInfo>::iterator p)
    {
        p--;
        while(p->type == NEXTLINE)
        {
            p--;
            line--;
        }
        return p;
    }

    /*
    <S
    G == 3>→begin <A>；<B> end
    <A>→<C><A*>              
    <A*>→；<C><A*>|ε
    <C>→integer <H>
    <H>→<I>│ function <I>（<I>）；<E>
    <I>→<L><I*>              
    <I*>→<G><I*>|<G>
   //c  <E>→begin <A>；<B> end
    <B>→<Z><B*>               
    <B*>→；<Z><B*>|<Z>
    <Z>→read(<I>)│write(<I>)│<I>:=<J>│if<K>then<Z>else <Z>
    <J>→<X><J*>              
    <J*>→ -<X><J*>|ε
    <X>→<Y><X*>               
    <X*>→ε|*<Y><X*>
    <Y>→<I>│<O>│<G>
    <K>→<J><P><J>
    <P>→<│<=│>│>=│=│<>      */
    void WriteFile(string msg)
    {

    }
    void WriteError(string msg)
    {
        error += "LINE:行号" + to_string(line) + "    " + msg + "\n";
    }

    list<Variable> varTable;
    list<SymInfo> sourceList;
    string error;
    string sym;
    string source;
    int line = 1;
    //list<SymInfo> lines;
};

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        cout << "use SyntacticAnalysis <dyd_file>" << endl;
    }
    ifstream source(argv[1]);
    if (!source.is_open())
    {
        ERROR_MSG("falied to open");
    }
    ofstream out("sout_test.dyd");
    if (!out.is_open())
    {
        ERROR_MSG("failed to open out");
    }

    ofstream error("serror_test.err");

    if (!error.is_open())
    {
        ERROR_MSG("open error file failed");
    }
    string data{istreambuf_iterator<char>{source}, istreambuf_iterator<char>{}};

    Analysis anlysisor = Analysis(data);
    anlysisor.StartAnalysis();

    anlysisor.dumpError(error);

    return 0;
}