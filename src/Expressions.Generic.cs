using System.Collections.Generic;
using System.Linq;
using System.Numerics;

// ReSharper disable once CheckNamespace
namespace DumbGrad.Expressions.Generic;

internal abstract class Expression<T>
    where T : IAdditionOperators<T, T, T>,
    IMultiplyOperators<T, T, T>,
    IMultiplyOperators<T, double, T>,
    IAdditionOperators<T, double, double> {
    public double Gradient { get; set; }

    public abstract T GetValue();

    public void Backward() {
        Gradient = 1;
        foreach (var dep in GetAllDependencies()) {
            dep.UpdateGradient();
        }
    }

    // Get expression this node depends on
    protected abstract IEnumerable<Expression<T>> GetDependencies();

    // Propagate gradient from this node to its dependencies
    protected abstract void UpdateGradient();

    private IReadOnlyList<Expression<T>> GetAllDependencies() {
        var result = new List<Expression<T>>();
        var visited = new HashSet<Expression<T>>();

        Loop(this);
        return result;

        void Loop(Expression<T> e) {
            if (visited.Add(e)) {
                foreach (var dep in e.GetDependencies()) {
                    Loop(dep);
                }

                result.Add(e);
            }
        }
    }

    public static Expression<T> operator +(Expression<T> left, Expression<T> right) => new Add<T>(left, right);

    public static Expression<T> operator *(Expression<T> left, Expression<T> right) => new Mult<T>(left, right);
}

internal class Value<T> : Expression<T>
    where T : IAdditionOperators<T, T, T>,
    IMultiplyOperators<T, T, T>,
    IMultiplyOperators<T, double, T>,
    IAdditionOperators<T, double, double> {
    private readonly T _value;

    public Value(T value) {
        _value = value;
    }

    public override T GetValue() => _value;

    protected override IEnumerable<Expression<T>> GetDependencies() => Enumerable.Empty<Expression<T>>();

    protected override void UpdateGradient() {
        // no-op
    }
}

internal class Add<T> : Expression<T>
    where T : IAdditionOperators<T, T, T>,
    IMultiplyOperators<T, T, T>,
    IMultiplyOperators<T, double, T>,
    IAdditionOperators<T, double, double> {
    private readonly Expression<T> _left;
    private readonly Expression<T> _right;

    public Add(Expression<T> left, Expression<T> right) {
        _left = left;
        _right = right;
    }

    protected override IEnumerable<Expression<T>> GetDependencies() {
        yield return _left;
        yield return _right;
    }

    public override T GetValue() => _left.GetValue() + _right.GetValue();

    protected override void UpdateGradient() {
        _left.Gradient += Gradient;
        _right.Gradient += Gradient;
    }
}

internal class Mult<T> : Expression<T>
    where T : IMultiplyOperators<T, T, T>,
    IMultiplyOperators<T, double, T>,
    IAdditionOperators<T, double, double>,
    IAdditionOperators<T, T, T> {
    private readonly Expression<T> _left;
    private readonly Expression<T> _right;

    public Mult(Expression<T> left, Expression<T> right) {
        _left = left;
        _right = right;
    }

    public override T GetValue() => _left.GetValue() * _right.GetValue();

    protected override IEnumerable<Expression<T>> GetDependencies() {
        yield return _left;
        yield return _right;
    }

    protected override void UpdateGradient() {
        // Must use `X = Y + X` form instead of `X += Y` because of IAdditionOperators<T, double, double>
        _left.Gradient = _right.GetValue() * Gradient + _left.Gradient;
        _right.Gradient = _left.GetValue() * Gradient + _right.Gradient;
    }
}