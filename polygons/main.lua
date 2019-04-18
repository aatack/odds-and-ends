require "marble"
require "plotter"

function love.load()
    t = 0
    fps = 0
    debugOpen = false
    debugOpened = false
    love.window.setFullscreen(true)

    dg = 0.01

    plotter = Plotter:new(0, 1, 0, 1, 100, love.graphics.getWidth() - 100, 100, love.graphics.getHeight() - 100)
    marble = Marble:new(function (x) return 0.5 - 0.3 * math.sin(3 + 8 * x - 1.5 * math.cos(t)) end)
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

    if love.keyboard.isDown("escape") then
        love.event.quit()
    end
    if love.keyboard.isDown("a") then
        marble.position = marble.position - dg
    end
    if love.keyboard.isDown("d") then
        marble.position = marble.position + dg
    end
    if love.keyboard.isDown("s") then
        -- marble:stepWithVelocity(0.005, 0.005, 0.99)
        marble:step(0.01)
    end
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
