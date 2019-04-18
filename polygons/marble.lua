Marble = {epsilon = 1e-4}

function Marble:new(loss, lossDerivative, lossSecondDerivative)
    o = {}
    setmetatable(o, self)
    self.__index = self
    o.loss = loss
    o.lossDerivative = lossDerivative or firstDerivative(loss)
    o.lossSecondDerivative = lossSecondDerivative or secondDerivative(loss)
    return o
end

function firstDerivative(f)
    local epsilon = Marble.epsilon
    return function(x)
        return 0.5 * (f(x + 0.5 * epsilon) - f(x - 0.5 * epsilon)) / epsilon
    end
end

function secondDerivative(f)
    local epsilon = Marble.epsilon
    return function(x)
        return (f(x + epsilon) - 2 * f(x) + f(x - epsilon)) / (epsilon * epsilon)
    end
end

function drawMarble(params)

end
