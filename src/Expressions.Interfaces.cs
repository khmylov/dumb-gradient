using System.Collections.Generic;
using System.Linq;

// ReSharper disable once CheckNamespace
namespace DumbGrad.Expressions.Interfaces;

public interface IExpression {
    double Gradient { get; set; }

    double GetValue();

    protected void UpdateGradient();

    protected IEnumerable<IExpression> GetDependencies();

    public void Backward() {
        Gradient = 1;
        foreach (var dep in GetAllDependencies()) {
            dep.UpdateGradient();
        }
    }

    private IReadOnlyList<IExpression> GetAllDependencies() {
        var result = new List<IExpression>();
        var visited = new HashSet<IExpression>();

        Loop(this);
        return result;

        void Loop(IExpression e) {
            if (visited.Add(e)) {
                foreach (var dep in e.GetDependencies()) {
                    Loop(dep);
                }
                result.Add(e);
            }
        }
    }

    static IExpression operator +(IExpression left, IExpression right) => new Add(left, right);
}

public struct Value : IExpression {
    private readonly double _value;

    public Value(double value) {
        _value = value;
    }

    public double Gradient { get; set; }

    public double GetValue() => _value;

    void IExpression.UpdateGradient() {
        // no-op
    }

    IEnumerable<IExpression> IExpression.GetDependencies() => Enumerable.Empty<IExpression>();
}

public class Add : IExpression {
    private readonly IExpression _left;
    private readonly IExpression _right;

    public Add(IExpression left, IExpression right) {
        _left = left;
        _right = right;
    }

    public double Gradient { get; set; }

    public double GetValue() => _left.GetValue() + _right.GetValue();

    void IExpression.UpdateGradient() {
        _left.Gradient += Gradient;
        _right.Gradient += Gradient;
    }

    IEnumerable<IExpression> IExpression.GetDependencies() {
        yield return _left;
        yield return _right;
    }
}