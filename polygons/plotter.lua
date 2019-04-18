Plotter = {}

function Plotter:new(xMin, xMax, yMin, yMax, left, right, top, bottom)
    o = {}
    setmetatable(o, self)
    self.__index = self

    self.xMin = xMin
    self.xMax = xMax
    self.yMin = yMin
    self.yMax = yMax

    self.xRange = xMax - xMin
    self.yRange = yMax - yMin

    self.top = top
    self.right = right
    self.bottom = bottom
    self.left = left

    self.width = right - left
    self.height = bottom - top

    return o
end

function Plotter:xCoord(x)
    return ((x - self.xMin) / self.xRange) * self.width + self.left
end

function Plotter:yCoord(y)
    return self.bottom - ((y - self.yMin) / self.yRange) * self.height
end

function Plotter:contains(point)
    local x = point[1]
    local y = point[2]
    return self.xMin <= x and x <= self.xMax and self.yMin <= y and y <= self.yMax
end

function Plotter:segment(start, finish)
    if self:contains(start) and self:contains(finish) then
        love.graphics.line(
            self:xCoord(start[1]), self:yCoord(start[2]), self:xCoord(finish[1]), self:yCoord(finish[2])
        )
    end
end

function Plotter:line(points)
    for i, point in ipairs(points) do
        if i ~= 1 then
            self:segment(points[i - 1], point)
        end
    end
end

function Plotter:plotFunction(f, params)
    local params = params or {}
    local points = {}
    local n = n or 100
    for x = params.lower or self.xMin,
            params.upper or self.xMax,
            params.step or self.xRange * 0.005 do
        table.insert(points, {x, f(x)})
    end
    self:line(points)
end

function Plotter:axes()
    if self.xMin <= 0 and 0 <= self.xMax then
        self:segment({0, self.yMin}, {0, self.yMax})
    end
    if self.yMin <= 0 and 0 <= self.yMax then
        self:segment({self.xMin, 0}, {self.xMax, 0})
    end
end
