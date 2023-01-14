using System.Collections.Generic;
using System.Linq;

// ReSharper disable once CheckNamespace
namespace DumbGrad.Expressions.Double;

// Abstract base class + inheritance implementation of expression nodes.

internal abstract class Expression {
    public double Gradient { get; set; }

    public abstract double GetValue();

    public void Backward() {
        Gradient = 1;
        foreach (var dep in GetAllDependencies()) {
            dep.UpdateGradient();
        }
    }

    // Get expression this node depends on
    protected abstract IEnumerable<Expression> GetDependencies();

    // Propagate gradient from this node to its dependencies
    protected abstract void UpdateGradient();

    private IReadOnlyList<Expression> GetAllDependencies() {
        var result = new List<Expression>();
        var visited = new HashSet<Expression>();

        Loop(this);
        return result;

        void Loop(Expression e) {
            if (visited.Add(e)) {
                foreach (var dep in e.GetDependencies()) {
                    Loop(dep);
                }
                result.Add(e);
            }
        }
    }

    public static Expression operator +(Expression left, Expression right) => new Add(left, right);

    public static Expression operator *(Expression left, Expression right) => new Mult(left, right);
}

internal class Value : Expression {
    private readonly double _value;

    public Value(double value) {
        _value = value;
    }

    public override double GetValue() => _value;

    protected override IEnumerable<Expression> GetDependencies() => Enumerable.Empty<Expression>();

    protected override void UpdateGradient() {
        // no-op
    }
}

internal class Add : Expression {
    private readonly Expression _left;
    private readonly Expression _right;

    public Add(Expression left, Expression right) {
        _left = left;
        _right = right;
    }

    protected override IEnumerable<Expression> GetDependencies() {
        yield return _left;
        yield return _right;
    }

    public override double GetValue() => _left.GetValue() + _right.GetValue();

    protected override void UpdateGradient() {
        _left.Gradient += Gradient;
        _right.Gradient += Gradient;
    }
}

internal class Mult : Expression {
    private readonly Expression _left;
    private readonly Expression _right;

    public Mult(Expression left, Expression right) {
        _left = left;
        _right = right;
    }

    public override double GetValue() => _left.GetValue() * _right.GetValue();

    protected override IEnumerable<Expression> GetDependencies() {
        yield return _left;
        yield return _right;
    }

    protected override void UpdateGradient() {
        _left.Gradient += _right.GetValue() * Gradient;
        _right.Gradient += _left.GetValue() * Gradient;
    }
}