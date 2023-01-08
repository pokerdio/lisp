#include <SPI.h>

#define CS 7
#define DECODE_MODE 9
#define INTENSITY 10
#define SCAN_LIMIT 11
#define SHUTDOWN 12
#define DISPLAY_TEST 15

void SendData(uint8_t address, uint8_t value) {
    digitalWrite(CS, LOW);
    SPI.transfer(address);
    SPI.transfer(value);
    digitalWrite(CS, HIGH);
}

void NewTetro (void);

#define JOY_CLICK 9
#define JOY_Y A1
#define JOY_X A2

#define BOARD_WIDTH 8
#define BOARD_HEIGHT 8

#define STATE_TRANSITION 1
#define STATE_GAMEOVER 2
#define STATE_GAME 3


#define TRANSITION_CYCLE 3
#define DOWN_CYCLE 40

#define HORIZ_CYCLE 12
#define HORIZ_SHORT_CYCLE 5
#define HORIZ_LIGHT 350
#define HORIZ_FULL 450

#define ROTATE_CYCLE 12
#define ROTATE_SHORT_CYCLE 5
#define ROTATE_LIGHT 350
#define ROTATE_FULL 450

struct {
    uint8_t board[BOARD_HEIGHT][BOARD_WIDTH];

    uint8_t state;


    int transition_timer; 
    uint8_t transition_state;

    uint8_t tetro_type;
    uint8_t tetro_rot;
    uint8_t tetro_cx;
    uint8_t tetro_cy; 

    uint8_t last_push; 

    int8_t x_state;
    int x_timer; 

    int8_t y_state;
    int y_timer;

    int down_timer; 
} game; 


#define TETR_S 1
#define TETR_Z 2
#define TETR_I 3
#define TETR_L 4
#define TETR_L2 5
#define TETR_SQ 6
#define TETR_T 7

int RandTetro (void) {
    return random(1, 8);
}

void PackXY(int (*xy)[4][2], int x1, int y1, int x2, int y2, int x3, int y3) {
    (*xy)[1][0] = x1;
    (*xy)[1][1] = y1;
    (*xy)[2][0] = x2;
    (*xy)[2][1] = y2;
    (*xy)[3][0] = x3;
    (*xy)[3][1] = y3;
}

void GenerateTetroXY(int type, int rot, int cx, int cy, int (*xy)[4][2]) {
    rot = (4 + (rot % 4)) % 4; 
    (*xy)[0][0] = cx;
    (*xy)[0][1] = cy;

    switch (type) {
    case TETR_S:
        switch (rot % 2) {
        case 0:
//  X   
//  0X
//   X
            PackXY(xy, cx, cy - 1, cx + 1, cy, cx + 1, cy + 1);
            break;
        case 1:
//  0X
// XX
            PackXY(xy, cx + 1, cy, cx - 1, cy + 1, cx, cy + 1);
            break;
        }
        break;
    case TETR_Z:
        switch (rot % 2) {
        case 0:
//   X   
//  0X
//  X
            PackXY(xy, cx + 1, cy - 1, cx + 1, cy, cx, cy + 1);
            break;
        case 1:
// X0
//  XX
            PackXY(xy, cx - 1, cy, cx, cy + 1, cx + 1, cy + 1);
            break;
        }
        break;
    case TETR_SQ:
        PackXY(xy, cx + 1, cy, cx, cy + 1, cx + 1, cy + 1);
        break;
    case TETR_I:
        switch (rot % 2) {
        case 0:
            PackXY(xy, cx, cy - 1, cx, cy + 1, cx, cy + 2);
            break;
        case 1:
            PackXY(xy, cx - 1, cy, cx + 1, cy, cx + 2, cy);
            break;
        }
        break;
    case TETR_L:
        switch (rot) {
        case 0:
// X
// X0X
            PackXY(xy, cx - 1, cy - 1, cx - 1, cy, cx + 1, cy);
            break;
        case 1:
// XX
// 0
// X
            PackXY(xy, cx, cy - 1, cx, cy + 1, cx + 1, cy - 1);
            break;
        case 2:
// X0X
//   X
            PackXY(xy, cx - 1, cy, cx + 1, cy, cx + 1, cy + 1);
            break;
        case 3:
//  X
//  0
// XX
            PackXY(xy, cx, cy - 1, cx - 1, cy + 1, cx, cy + 1);
            break;
        }
        break;
    case TETR_L2:
        switch (rot) {
        case 0:
//   X
// X0X
            PackXY(xy, cx + 1, cy - 1, cx - 1, cy, cx + 1, cy);
            break;
        case 1:
// X
// 0
// XX
            PackXY(xy, cx, cy - 1, cx, cy + 1, cx + 1, cy + 1);
            break;
        case 2:
// X0X
// X

            PackXY(xy, cx - 1, cy, cx + 1, cy, cx - 1, cy + 1);
            break;
        case 3:
// XX
//  0
//  X
            PackXY(xy, cx, cy - 1, cx - 1, cy - 1, cx, cy + 1);
            break;
        }
        break;
    case TETR_T:
        switch (rot) {
        case 0:
//  X
// X0X
            PackXY(xy, cx, cy - 1, cx - 1, cy, cx + 1, cy);
            break;
        case 1:
// X
// 0X
// X
            PackXY(xy, cx, cy - 1, cx, cy + 1, cx + 1, cy);
            break;
        case 2:
// X0X
//  X

            PackXY(xy, cx - 1, cy, cx + 1, cy, cx, cy + 1);
            break;
        case 3:
//  X
// X0
//  X
            PackXY(xy, cx, cy - 1, cx - 1, cy, cx, cy + 1);
            break;
        }
        break;
    }
}

void ResetBoard(uint8_t value = 0) {
    memset(game.board, value, sizeof(game.board));
}

void ResetGame() {
    Serial.println("game reset");
    ResetBoard ();
    game.state = STATE_TRANSITION;
    game.transition_state = 21;
    game.transition_timer = TRANSITION_CYCLE;
    game.last_push = 0; 
}

void DrawBoard() {
    for (int col=0 ; col<8 ; ++col) {
        int data = 0;
        int row = 0;
        for (int i=128 ; i>0 ; i/=2) {
            if (game.board[BOARD_HEIGHT - 1 - row++][col]) {
                data += i;
            }
        }
        SendData(col + 1, data);    
    } 
}

void TransitionTick() {
    if (game.state != STATE_TRANSITION)  {
        return;
    }

    if (--game.transition_timer > 0) {
        return;
    }

    Serial.print ("transition tick ");
    Serial.println (game.transition_state);

    ResetBoard();

    if (0 >= game.transition_state) {
        Serial.print("new tetro!");
        game.state = STATE_GAME; 
        NewTetro ();
        return; 
    }
    -- game.transition_state;
    int depth;
    for (int col=0 ; col<4 ; ++col) {
        for (int row=0 ; row<4 ; ++row) {
            depth = max (3 - row, 3 - col);
            if (((depth + game.transition_state + 1) % 4) == 0) {
                game.board[row][col] = 1;
                game.board[row][7 - col] = 1;
                game.board[7 - row][col] = 1;
                game.board[7 - row][7 - col] = 1;
            }
        }
    }
    game.transition_timer = TRANSITION_CYCLE;
}

int XYCheck(int (*xy)[4][2]) {
    for (int i=0 ; i<4 ; ++i) {
        int x = (*xy)[i][0]; 
        int y = (*xy)[i][1]; 
        if (x < 0 || x >= BOARD_WIDTH || x < 0 || y >= BOARD_HEIGHT) {
            return 0; 
        }
        if (game.board[y][x] != 0) {
            return 0; 
        }
    }
    return 1; 
}

void StampTetro(int (*xy)[4][2], uint8_t value) {
    Serial.print("stamping ");

    Serial.print(value);
    Serial.print(" at ");
    for (int i=0 ; i<4 ; ++i) {
        int x = (*xy)[i][0];
        int y = (*xy)[i][1];
        Serial.print(x);
        Serial.print(":");
        Serial.print(y);
        Serial.print(" ");
        if (x >= 0 && x < BOARD_WIDTH && y >= 0 && y < BOARD_HEIGHT) {
            game.board[y][x] = value;
        }
    }
    Serial.println("");
}

int TetroSpawnY(int type) {
    switch(type) {
    case TETR_S:
    case TETR_Z:
    case TETR_I:
    case TETR_L:
    case TETR_L2:
    case TETR_T:
        return 1;
    case TETR_SQ:
        return 0;
    }
}

void NewTetro (void) {
    int type = RandTetro();

    int cx = 3, cy = TetroSpawnY(type);
    int xy[4][2];
    GenerateTetroXY(type, 0, cx, cy, &xy);

    if (XYCheck(&xy)) {
        game.tetro_type = type;
        game.tetro_rot = 0;
        game.tetro_cx = cx;
        game.tetro_cy = cy; 
        game.state = STATE_GAME;
        game.down_timer = DOWN_CYCLE;
    } else {
        game.state = STATE_GAMEOVER; 
    }
    StampTetro(&xy, 1);
}

// the setup function runs once when you press reset or power the board
void setup() {
    Serial.begin(9600);           // set up Serial library at 9600 bps
    Serial.print("initializing random seed: ");
    long seed = analogRead(0);

    Serial.println(seed);
    randomSeed(seed);

    // initialize digital pin LED_BUILTIN as an output.
    pinMode(CS, OUTPUT);
    SPI.setBitOrder(MSBFIRST);
    SPI.setClockDivider(SPI_CLOCK_DIV4);
    SPI.begin();
    pinMode(JOY_CLICK, INPUT_PULLUP);
    pinMode(JOY_X, INPUT_PULLUP);
    pinMode(JOY_Y, INPUT_PULLUP);  

    SendData(DISPLAY_TEST, 0x01);
    delay(1000);
    SendData(DISPLAY_TEST, 0x00);
    SendData(DECODE_MODE, 0x00);
    SendData(INTENSITY, 0x03);
    SendData(SCAN_LIMIT, 0x0f);
    SendData(SHUTDOWN, 0x01);

    ResetGame();
}

void Rotate(int delta) {
    int xyold[4][2];
    int xynew[4][2];

    GenerateTetroXY(game.tetro_type, game.tetro_rot, game.tetro_cx, game.tetro_cy, &xyold);
    StampTetro(&xyold, 0);

    game.tetro_rot += delta;
    GenerateTetroXY(game.tetro_type, game.tetro_rot, game.tetro_cx, game.tetro_cy, &xynew);
    
    if (XYCheck(&xynew)) {
        StampTetro(&xynew, 1);
    } else {
        game.tetro_rot -= delta;
        StampTetro(&xyold, 1);
    }
}


void Horiz(int delta) {
    int xyold[4][2];
    int xynew[4][2];

    GenerateTetroXY(game.tetro_type, game.tetro_rot, game.tetro_cx, game.tetro_cy, &xyold);
    StampTetro(&xyold, 0);

    game.tetro_cx += delta;
    GenerateTetroXY(game.tetro_type, game.tetro_rot, game.tetro_cx, game.tetro_cy, &xynew);
    
    if (XYCheck(&xynew)) {
        StampTetro(&xynew, 1);
    } else {
        game.tetro_cx -= delta;
        StampTetro(&xyold, 1);
    }
}

void XTick(int x) {
    if (abs(x) < HORIZ_LIGHT && game.x_state != 0) {
        game.x_state = 0;
        Serial.println("X NULL");
        return;
    }
    if (x >= HORIZ_FULL && game.x_state < 1) {
        Serial.println("RIGHT");
        Horiz(1);

        game.x_state = 1;
        game.x_timer = HORIZ_CYCLE; 
        return; 
    }
    if (x <= -HORIZ_FULL && game.x_state > -1) {
        Serial.print(game.x_state);
        Serial.println("LEFT");
        Horiz(-1);
        game.x_state = -1;
        game.x_timer = HORIZ_CYCLE; 
        return; 
    }

    game.x_timer--; 
    if (x > HORIZ_FULL && game.x_state >= 1 && game.x_timer <= 0) {
        game.x_state = 2;
        Serial.println("RIGHT repeatable");
        Horiz(1);
        game.x_timer = HORIZ_SHORT_CYCLE;
        return;
    }
    if (x < -HORIZ_FULL && game.x_state <= -1 && game.x_timer <= 0) {
        game.x_state = -2;
        Serial.println("LEFT repeatable");
        Horiz(-1);
        game.x_timer = HORIZ_SHORT_CYCLE;
        return;
    }
}

void RotateTick(int y) {
    if (abs(y) < ROTATE_LIGHT && game.y_state != 0) {
        game.y_state = 0;
        return;
    }
    if (y >= ROTATE_FULL && game.y_state < 1) {
        Rotate(1);

        game.y_state = 1;
        game.y_timer = ROTATE_CYCLE; 
        return; 
    }
    if (y <= -ROTATE_FULL && game.y_state > -1) {
        Rotate(-1);
        game.y_state = -1;
        game.y_timer = ROTATE_CYCLE; 
        return; 
    }

    game.y_timer--; 
    if (y > ROTATE_FULL && game.y_state >= 1 && game.y_timer <= 0) {
        game.y_state = 2;
        Rotate(1);
        game.y_timer = ROTATE_SHORT_CYCLE;
        return;
    }
    if (y < -ROTATE_FULL && game.y_state <= -1 && game.y_timer <= 0) {
        game.y_state = -2;
        Rotate(-1);
        game.y_timer = ROTATE_SHORT_CYCLE;
        return;
    }
}

void DeleteFullLines() {
    for (int y=BOARD_HEIGHT-1 ; y>=0 ; --y) {
        int count = 0; 
        for (int x=0 ; x<BOARD_WIDTH ; ++x) {
            count += game.board[y][x];
        }
        if (count == BOARD_WIDTH) {
            for (int y2=y ; y2>0 ; --y2) {
                for (int x=0 ; x<BOARD_WIDTH ; ++x) {
                    game.board[y2][x] = game.board[y2 - 1][x];
                }
            }
            for (int x=0 ; x<BOARD_WIDTH ; ++x) {
                game.board[0][x] = 0; 
            }
        }
    }
}

void DownTick(void) {
    int xyold[4][2];
    int xynew[4][2];

    if (--game.down_timer > 0) {
        return;
    }
    game.down_timer = DOWN_CYCLE;

    GenerateTetroXY(game.tetro_type, game.tetro_rot, game.tetro_cx, game.tetro_cy, &xyold);
    StampTetro(&xyold, 0);

    game.tetro_cy += 1;
    GenerateTetroXY(game.tetro_type, game.tetro_rot, game.tetro_cx, game.tetro_cy, &xynew);
    
    if (XYCheck(&xynew)) {
        StampTetro(&xynew, 1);
    } else {
        game.tetro_cy -= 1;
        StampTetro(&xyold, 1);
        DeleteFullLines();
        NewTetro ();
    }
}

// the loop function runs over and over again forever
void loop() {
    long analogX, analogY, push;  
  
    analogY = (long)analogRead(JOY_X) - 512; // reversed for personal convenience
    analogX = (long)analogRead(JOY_Y) - 512; 
    push = digitalRead(JOY_CLICK);

    switch (game.state) {
    case STATE_TRANSITION:
        TransitionTick();
        break;
    case STATE_GAME:
        XTick(analogX);
        RotateTick(analogY);
        DownTick();
        break;
    case STATE_GAMEOVER:
        if (!push) {
            ResetGame();
        }
        ResetBoard(1);
        break;
    }
    DrawBoard();
    delay(20);
}
