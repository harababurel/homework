const koa = require('koa');
const app = new koa();
const server = require('http').createServer(app.callback());
const Router = require('koa-router');
const cors = require('koa-cors');
const bodyParser = require('koa-bodyparser');
const convert = require('koa-convert');

app.use(bodyParser());
app.use(convert(cors()));
app.use(async(ctx, next) => {
    const start = new Date();
    await next();
    const ms = new Date() - start;
    console.log(`${ctx.method} ${ctx.url} ${ctx.response.status} - ${ms}ms`);
});

const seatTypes = ['stall', 'royal circle', 'grand circle', 'balcony'];
const statusTypes = ['available', 'reserved', 'confirmed', 'taken'];
const seats = [];
for (let i = 0; i < 10; i++) {
    seats.push({
        id: i + 1,
        name: "#" + (i + 1),
        type: seatTypes[0],
        status: statusTypes[0]
    });
    seats.push({
        id: i + 11,
        name: "#" + (i + 11),
        type: seatTypes[1],
        status: statusTypes[0]
    });
    seats.push({
        id: i + 21,
        name: "#" + (i + 21),
        type: seatTypes[2],
        status: statusTypes[0]
    });
    seats.push({
        id: i + 31,
        name: "#" + (i + 31),
        type: seatTypes[3],
        status: statusTypes[0]
    });
}

const router = new Router();
router.get('/all', ctx => {
    ctx.response.body = seats;
    ctx.response.status = 200;
});

router.get('/confirmed', ctx => {
    ctx.response.body = seats.filter(seat => seat.status === statusTypes[2]);
    ctx.response.status = 200;
});

router.get('/taken', ctx => {
    ctx.response.body = seats.filter(seat => seat.status === statusTypes[3]);
    ctx.response.status = 200;
});

router.get('/seats', ctx => {
    ctx.response.body = seats.filter(seat => seat.status === statusTypes[0]);
    ctx.response.status = 200;
});

router.post('/reserve', ctx => {
    // console.log("ctx: " + JSON.stringify(ctx));
    const headers = ctx.request.body;
    console.log("body: " + JSON.stringify(headers));
    const id = headers.id;
    if (typeof id !== 'undefined') {
        const index = seats.findIndex(seat => seat.id == id && seat.status === statusTypes[0]);
        if (index === -1) {
            console.log("Seat not available");
            ctx.response.body = {text: 'Seat not available!'};
            ctx.response.status = 404;
        } else {
            let seat = seats[index];
            seat.status = statusTypes[1];
            ctx.response.body = seat;
            ctx.response.status = 200;
        }
    } else {
        console.log("Missing id or invalid");
        ctx.response.body = {text: 'Missing id or invalid'};
        ctx.response.status = 404;
    }
});

router.del('/clean', ctx => {
    console.log("init all the seats.");
    seats.map(seat => seat.status = statusTypes[0]);
    ctx.response.body = {text: 'Init done.'};
    ctx.response.status = 200;
});

router.del('/zap', ctx => {
    console.log("zap all the seats.");
    seats.splice(0, seats.length);
    ctx.response.body = {text: 'Zap done.'};
    ctx.response.status = 200;
});


router.get('/refresh/:id', ctx => {
    // console.log("ctx: " + JSON.stringify(ctx));
    const headers = ctx.params;
    console.log("body: " + JSON.stringify(headers));
    const id = headers.id;
    if (typeof id !== 'undefined') {
        const index = seats.findIndex(seat => seat.id == id);
        if (index === -1) {
            console.log("No seat with id: " + id);
            ctx.response.body = {text: 'Invalid id'};
            ctx.response.status = 404;
        } else {
            ctx.response.body = seats[index];
            ctx.response.status = 200;
        }
    } else {
        ctx.response.body = {text: 'Id missing or invalid'};
        ctx.response.status = 404;
    }
});

router.post('/buy', ctx => {
    const headers = ctx.request.body;
    console.log("body: " + JSON.stringify(headers));
    const id = headers.id;
    if (typeof id !== 'undefined') {
        const index = seats.findIndex(seat => seat.id == id && seat.status === statusTypes[2]);
        if (index === -1) {
            ctx.response.body = {text: 'Seat not available!'};
            ctx.response.status = 404;
        } else {
            let seat = seats[index];
            seat.status = statusTypes[3];
            ctx.response.body = seat;
            ctx.response.status = 200;
        }
    } else {
        ctx.response.body = {text: 'Missing id or invalid'};
        ctx.response.status = 404;
    }
});

router.post('/confirm', ctx => {
    const headers = ctx.request.body;
    console.log("body: " + JSON.stringify(headers));
    const id = headers.id;
    if (typeof id !== 'undefined') {
        const index = seats.findIndex(seat => seat.id == id && seat.status === statusTypes[1]);
        if (index === -1) {
            ctx.response.body = {text: 'Seat not available'};
            ctx.response.status = 404;
        } else {
            let seat = seats[index];
            seat.status = statusTypes[2];
            ctx.response.body = seat;
            ctx.response.status = 200;
        }
    } else {
        ctx.response.body = {text: 'Missing id or invalid'};
        ctx.response.status = 404;
    }
});


app.use(router.routes());
app.use(router.allowedMethods());

server.listen(4021);
