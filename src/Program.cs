{
    var V = DumbGrad.Expr.Value<double>;

    var (a, b, c) = (V(2), V(-3), V(100));
    var e = a * b;
    System.Console.WriteLine(e.GetValue());

    // var V1 = Expr.Value<int>;

}

{
    var e = new DumbGrad.Expressions.Generic.Value<double>(23);
    var e2 = e + e;
}

{
    DumbGrad.Expressions.Interfaces.IExpression e = new DumbGrad.Expressions.Interfaces.Value(23);
    var e2 = e + e;
}
