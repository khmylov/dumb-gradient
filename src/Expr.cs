using System;
using System.Collections.Generic;
using System.Numerics;

namespace DumbGrad;

internal class Expr<T> where T : INumber<T>, IMultiplyOperators<T, double, double> {
    private readonly Func<Expr<T>, T> _getValueImpl;
    private readonly Action<Expr<T>> _updateGradient;

    /// <remarks>
    /// Function parameters are expected to be static, so that each expression instance gets the same delegate, to avoid additional GC pressure.
    /// </remarks>
    public Expr(
        IReadOnlyList<Expr<T>> dependencies,
        Func<Expr<T>, T> getValueImpl,
        Action<Expr<T>> updateGradient,
        // Used only as a hack to pass the underlying value to Value implementation and avoid closure allocations
        T? dummyValue = default) {
        _getValueImpl = getValueImpl;
        _updateGradient = updateGradient;
        Dependencies = dependencies;
        DummyValue = dummyValue;
    }

    public double Gradient { get; private set; }
    public IReadOnlyList<Expr<T>> Dependencies { get; }
    public T? DummyValue { get; }

    public T GetValue() => _getValueImpl(this);

    public void AddGradient(double value) {
        Gradient += value;
    }

    public void Backward() {
        Gradient = 1;
        foreach (var dep in GetAllDependencies()) {
            dep._updateGradient(dep);
        }
    }

    private IReadOnlyList<Expr<T>> GetAllDependencies() {
        var result = new List<Expr<T>>();
        var visited = new HashSet<Expr<T>>();

        Loop(this);
        return result;

        void Loop(Expr<T> e) {
            if (visited.Add(e)) {
                foreach (var dep in e.Dependencies) {
                    Loop(dep);
                }
                result.Add(e);
            }
        }
    }

    public static Expr<T> operator +(Expr<T> left, Expr<T> right) {
        return new Expr<T>(
            // TODO: would be great to avoid array allocations
            new[] { left, right },
            static e => e.Dependencies[0].GetValue() + e.Dependencies[1].GetValue(),
            static e => {
                e.Dependencies[0].AddGradient(e.Gradient);
                e.Dependencies[1].AddGradient(e.Gradient);
            });
    }

    public static Expr<T> operator *(Expr<T> left, Expr<T> right) => new(
        new[] { left, right },
        static e => e.Dependencies[0].GetValue() * e.Dependencies[1].GetValue(),
        static e => {
            e.Dependencies[0].AddGradient(e.Dependencies[1].GetValue() * e.Gradient);
            e.Dependencies[1].AddGradient(e.Dependencies[0].GetValue() * e.Gradient);
        });
}

internal static class Expr {
    public static Expr<T> Value<T>(T value) where T : INumber<T>, IMultiplyOperators<T, double, double> => new(
        Array.Empty<Expr<T>>(),
        static e => e.DummyValue!,
        static _ => { },
        value);

    public static Expr<T> Tanh<T>(this Expr<T> x) where T :
        INumber<T>,
        IHyperbolicFunctions<T>,
        IMultiplyOperators<T, double, double>,
        IAdditionOperators<T, double, double> => new(
        new[] { x },
        static e => T.Tanh(e.Dependencies[0].GetValue()),
        static e => {
            var value = e.GetValue(); // = tanh(x)
            // 1 - tanh(x)^2
            e.Dependencies[0].AddGradient((-value * value + 1.0) * e.Gradient);
        });
}