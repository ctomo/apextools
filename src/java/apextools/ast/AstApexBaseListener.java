package apextools.ast;

import apex.parser.ApexBaseListener;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.TerminalNode;


public class AstApexBaseListener extends ApexBaseListener {

    protected Object root;
    protected ParseTreeProperty<Object> treeProperty = new ParseTreeProperty<>();

    public Object getRoot() {
        return root;
    }

    public void setTreeProperty(ParserRuleContext ctx, Object obj) {
        treeProperty.put(ctx, obj);
    }

    public Object getTreeProperty(ParserRuleContext ctx) {
        return treeProperty.get(ctx);
    }
    
    @Override
    public void exitEveryRule(ParserRuleContext ctx) {
        // Compress paths
        if (ctx.getChildCount() == 1
                && !(ctx.getChild(0) instanceof TerminalNode)
                && treeProperty.get(ctx) == null) {
            treeProperty.put(ctx, treeProperty.get(ctx.getChild(0)));
        }

        // when finished, we are at the root of the syntax tree
        root = treeProperty.get(ctx);
    }
}
