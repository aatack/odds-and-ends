require "marble"
require "plotter"

function love.load()
    t = 0
    fps = 0
    debugOpen = false
    debugOpened = false
    love.window.setFullscreen(true)

    dg = 0.01

    plotter = Plotter:new(0, 1, 0, 1, 100,
        love.graphics.getWidth() - 100, 100,
        love.graphics.getHeight() - 100)
    marble = Marble:new(
        function(x)
            return 0.5 - 0.3 * math.sin(3 + 8 * x - 1.5 * math.cos(t))
        end
    )
end

function love.update(dt)
    t = t + dt
    fps = math.floor(1 / dt)

    if love.keyboard.isDown("f3") then
        if not debugOpened then
            debugOpen = not debugOpen
            debugOpened = true
        end
    else
        debugOpened = false
    end

    for k, f in pairs(held) do
        if heldKeys[k] ~= nil then f() end
    end
end

pressed = {
    escape = function () love.event.quit() end,
}
released = {}

heldKeys = {}
held = {
    a = function () marble.position = marble.position - dg end,
    d = function () marble.position = marble.position + dg end,
    s = function () marble:step(0.01) end,
}

function love.keypressed(key, _, _)
    if pressed[key] ~= nil then
        pressed[key]()
    end
    heldKeys[key] = true
end

function love.keyreleased(key, _, _)
    if released[key] ~= nil then
        released[key]()
    end
    heldKeys[key] = nil
end

function love.draw()
    if debugOpen then
        love.graphics.print("Time: " .. t, 5, 5)
        love.graphics.print("FPS: " .. fps, 5, 25)
    end

    marble:plot(plotter)
end

function love.conf(game)
    game.console = true
end
