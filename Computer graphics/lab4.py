import glfw
from OpenGL.GL import *

win_size_x = 1400
win_size_y = 900
matrix = [[154] * win_size_x for _ in range(win_size_y)]
points = []
intersections = [0] * win_size_y
color = 1
N = 3


def filt():
    global matrix
    n = N
    for i in range(n, win_size_y - n):
        for j in range(n, win_size_x - n):
            s = matrix[i + n][j - n] + matrix[i + n][j] + matrix[i + n][j + n] + matrix[i][j - n] + matrix[i][j] + \
                matrix[i][j + n] + matrix[i - n][j - n] + matrix[i - n][j] + matrix[i - n][j + n]
            if s > 0:
                matrix[i][j] = int((1 * matrix[i + n][j - n] + 2 * matrix[i + n][j] + 1 * matrix[i + n][j + n] + 2 *
                                    matrix[i][j - n] + 4 * matrix[i][j] + 2 * matrix[i][j + n] + 1 * matrix[i - n][
                                        j - n] + 2 * matrix[i - n][j] + 1 * matrix[i - n][j + n]) / 16)
            else:
                matrix[i][j] = 0


def fill():
    Y_ = []
    X_ = []
    for point in points:
        Y_.append(point[1])
        X_.append(point[0])
    for i in range(win_size_y):
        in_ = False
        j = 0
        black = matrix[i].count(0)
        if black == 1:
            continue
        while j < win_size_x - 2:
            if matrix[i][j + 1] == 0 and not in_:
                in_ = True
                d = 0
                while matrix[i][j + 1] == 0:
                    if not (0 in matrix[i][j + 1:]):
                        break
                    d += 1
                    j += 1
                if not (0 in matrix[i][j + 1:]):
                    break

            elif matrix[i][j + 1] == 0 and in_:
                in_ = False
                d = 0
                while matrix[i][j + 1] == 0:
                    d += 1
                    j += 1

                cond = d < 3 and (j in X_)
                if ([j, i] in points or cond) and (0 in matrix[i][0:j]) and (0 in matrix[i][j + 1:]):
                    in_ = True
            if in_:
                matrix[i][j + 1] = 0
            else:
                matrix[i][j + 1] = 255
            j += 1


def drawLine(x0, y0, x1, y1):
    global intersections
    if x0 == x1:
        m = 100000
    else:
        m = (y1 - y0) / (x1 - x0)
    e = -1 / 2
    x = x0
    y = y0
    is_sharp = True
    if x <= x1 and y <= y1:
        if m > 1:
            is_sharp = False
            m = 1 / m
        while x <= x1 and y <= y1:
            matrix[y][x] = 0
            if is_sharp:
                x += 1
            else:
                intersections[y] += 1
                y += 1
            e += m
            if e >= 0:
                if is_sharp:
                    intersections[y] += 1
                    y += 1
                else:
                    x += 1
                e -= 1
    elif x >= x1 and y <= y1:
        m = -m
        if m > 1:
            is_sharp = False
            m = 1 / m
        while x >= x1 and y <= y1:
            matrix[y][x] = 0
            if is_sharp:
                x -= 1
            else:
                intersections[y] += 1
                y += 1
            e += m
            if e >= 0:
                if is_sharp:
                    intersections[y] += 1
                    y += 1
                else:
                    x -= 1
                e -= 1
    elif x >= x1 and y >= y1:
        if m > 1:
            is_sharp = False
            m = 1 / m
        while x >= x1 and y >= y1:
            matrix[y][x] = 0
            if is_sharp:
                x -= 1
            else:
                intersections[y] += 1
                y -= 1
            e += m
            if e >= 0:
                if is_sharp:
                    intersections[y] += 1
                    y -= 1
                else:
                    x -= 1
                e -= 1
    elif x <= x1 and y >= y1:
        m = -m
        if m > 1:
            m = 1 / m
            is_sharp = False
        while x <= x1 and y >= y1:
            matrix[y][x] = 0
            if is_sharp:
                x += 1
            else:
                intersections[y] += 1
                y -= 1
            e += m
            if e >= 0:
                if is_sharp:
                    intersections[y] += 1
                    y -= 1
                else:
                    x += 1
                e -= 1


def display(window):
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glLoadIdentity()
    glClearColor(0, 0, 0, 0)
    glColor3f(1.0, 1.0, 1.0)
    glRasterPos(-1, -1)
    glPixelZoom(2, 2)
    glDrawPixels(win_size_x, win_size_y, GL_LUMINANCE, GL_UNSIGNED_BYTE, matrix)
    glfw.swap_buffers(window)
    glfw.poll_events()


def mouse_button_callback(window, button, action, mods):
    global points
    if button == glfw.MOUSE_BUTTON_LEFT and action == glfw.PRESS:
        t = glfw.get_cursor_pos(window)
        t = list(t)
        t[0] = int(t[0])
        t[1] = int(700 - t[1])
        if len(points) > 0:
            drawLine(points[-1][0], points[-1][1], t[0], t[1])
            intersections[points[-1][1]] -= 1
        points.append(t)


def key_callback(window, key, scancode, action, mods):
    global matrix
    if key == glfw.KEY_SPACE and action == glfw.PRESS:
        drawLine(points[-1][0], points[-1][1], points[0][0], points[0][1])
        intersections[points[0][1]] -= 1
        intersections[points[-1][1]] -= 1
    if key == glfw.KEY_F and action == glfw.PRESS:
        fill()
    if key == glfw.KEY_E and action == glfw.PRESS:
        filt()


def main():
    if not glfw.init():
        return
    window = glfw.create_window(win_size_x, win_size_y, "Lab4", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    glfw.set_mouse_button_callback(window, mouse_button_callback)
    while not glfw.window_should_close(window):
        display(window)
    glfw.destroy_window(window)
    glfw.terminate()


if __name__ == '__main__':
    main()
