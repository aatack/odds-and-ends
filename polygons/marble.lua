Marble = {
    position = 0,
    velocity = 0,
    epsilon = 1e-4
}

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
        return (f(x + 0.5 * epsilon) - f(x - 0.5 * epsilon)) / epsilon
    end
end

function secondDerivative(f)
    local epsilon = Marble.epsilon
    return function(x)
        return (f(x + epsilon) - 2 * f(x) + f(x - epsilon)) / (epsilon * epsilon)
    end
end

function Marble:firstOrderApproximation(pivot)
    local slope = self.lossDerivative(pivot)
    local base = self.loss(pivot)
    return function(x)
        local dx = x - pivot
        return base + dx * slope
    end
end

function Marble:secondOrderApproximation(pivot)
    local curve = self.lossSecondDerivative(pivot)
    local slope = self.lossDerivative(pivot)
    local base = self.loss(pivot)
    return function(x)
        local dx = x - pivot
        return base + dx * slope + dx * dx * curve
    end
end

function Marble:plot(plotter, axes)
    plotter:plotFunction(self.loss)
    if self.position ~= nil then
        local range = 0.1 * plotter.xRange
        local plotParams = {lower = self.position - range, upper = self.position + range}
        plotter:plotFunction(self:firstOrderApproximation(self.position), plotParams)
        plotter:plotFunction(self:secondOrderApproximation(self.position), plotParams)
    end
    if axes or false then
        plotter:axes()
    end
    if self.position ~= nil then
        plotter:dot({self.position, self.loss(self.position)})
    end
end

function Marble:step(learningRate)
    self.position = self.position - learningRate * self.lossDerivative(self.position)
end

function Marble:stepWithVelocity(vl, pl, damping)
    self.velocity = (self.velocity - vl * self.lossDerivative(self.position)) * damping
    self.position = self.position + pl * self.velocity
end
