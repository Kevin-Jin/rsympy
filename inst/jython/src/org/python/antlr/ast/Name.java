// Autogenerated AST node
package org.python.antlr.ast;
import org.python.antlr.PythonTree;
import org.antlr.runtime.CommonToken;
import org.antlr.runtime.Token;
import java.io.DataOutputStream;
import java.io.IOException;

public class Name extends exprType implements Context {
    public String id;
    public expr_contextType ctx;

    public static final String[] _fields = new String[] {"id","ctx"};

    public Name(Token token, String id, expr_contextType ctx) {
        super(token);
        this.id = id;
        this.ctx = ctx;
    }

    public Name(int ttype, Token token, String id, expr_contextType ctx) {
        super(ttype, token);
        this.id = id;
        this.ctx = ctx;
    }

    public Name(PythonTree tree, String id, expr_contextType ctx) {
        super(tree);
        this.id = id;
        this.ctx = ctx;
    }

    public String toString() {
        return "Name";
    }

    public String toStringTree() {
        StringBuffer sb = new StringBuffer("Name(");
        sb.append("id=");
        sb.append(dumpThis(id));
        sb.append(",");
        sb.append("ctx=");
        sb.append(dumpThis(ctx));
        sb.append(",");
        sb.append(")");
        return sb.toString();
    }

    public <R> R accept(VisitorIF<R> visitor) throws Exception {
        return visitor.visitName(this);
    }

    public void traverse(VisitorIF visitor) throws Exception {
    }

    public void setContext(expr_contextType c) {
        this.ctx = c;
    }

    public int getLineno() {
        return getLine();
    }

    public int getCol_offset() {
        return getCharPositionInLine();
    }

}
