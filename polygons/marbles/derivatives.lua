require "marbles.util"

Derivatives = {}

function Derivatives:new(params)
    local o = params or {}
    setmetatable(o, Derivatives)
    self.__index = self

    o.useHessian = o.useHessian or false

    return o
end

function approximateFirstDerivative(index, f, epsilon)
    -- Approximate the first derivative of a vector function in a particular
    -- dimension using a central difference method.
    local epsilon = epsilon or 1e-4
    return function (x)
        local copy = copyVector(x)
        copy[index] = copy[index] + epsilon * 0.5
        local above = f(copy)
        copy[index] = copy[index] - epsilon
        local below = f(copy)
        return (above - below) / epsilon
    end
end
