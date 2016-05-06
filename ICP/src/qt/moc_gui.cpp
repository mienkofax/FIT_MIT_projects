/****************************************************************************
** Meta object code from reading C++ file 'gui.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "gui.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'gui.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
struct qt_meta_stringdata_gui_t {
    QByteArrayData data[20];
    char stringdata0[386];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_gui_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_gui_t qt_meta_stringdata_gui = {
    {
QT_MOC_LITERAL(0, 0, 3), // "gui"
QT_MOC_LITERAL(1, 4, 24), // "on_buttonNewGame_clicked"
QT_MOC_LITERAL(2, 29, 0), // ""
QT_MOC_LITERAL(3, 30, 21), // "on_buttonBack_clicked"
QT_MOC_LITERAL(4, 52, 25), // "on_buttonLoadGame_clicked"
QT_MOC_LITERAL(5, 78, 38), // "on_comboBoxPlayer2_currentInd..."
QT_MOC_LITERAL(6, 117, 5), // "index"
QT_MOC_LITERAL(7, 123, 24), // "on_pushButton_12_clicked"
QT_MOC_LITERAL(8, 148, 14), // "moveToPosition"
QT_MOC_LITERAL(9, 163, 1), // "x"
QT_MOC_LITERAL(10, 165, 1), // "y"
QT_MOC_LITERAL(11, 167, 25), // "on_buttonNewGame2_clicked"
QT_MOC_LITERAL(12, 193, 22), // "on_buttonLoad2_clicked"
QT_MOC_LITERAL(13, 216, 21), // "on_buttonUndo_clicked"
QT_MOC_LITERAL(14, 238, 21), // "on_buttonRedo_clicked"
QT_MOC_LITERAL(15, 260, 21), // "on_buttonSave_clicked"
QT_MOC_LITERAL(16, 282, 27), // "on_buttonBackToGame_clicked"
QT_MOC_LITERAL(17, 310, 27), // "on_buttonChangeGame_clicked"
QT_MOC_LITERAL(18, 338, 25), // "on_buttonDemoGame_clicked"
QT_MOC_LITERAL(19, 364, 21) // "on_buttonPass_clicked"

    },
    "gui\0on_buttonNewGame_clicked\0\0"
    "on_buttonBack_clicked\0on_buttonLoadGame_clicked\0"
    "on_comboBoxPlayer2_currentIndexChanged\0"
    "index\0on_pushButton_12_clicked\0"
    "moveToPosition\0x\0y\0on_buttonNewGame2_clicked\0"
    "on_buttonLoad2_clicked\0on_buttonUndo_clicked\0"
    "on_buttonRedo_clicked\0on_buttonSave_clicked\0"
    "on_buttonBackToGame_clicked\0"
    "on_buttonChangeGame_clicked\0"
    "on_buttonDemoGame_clicked\0"
    "on_buttonPass_clicked"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_gui[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
      15,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags
       1,    0,   89,    2, 0x08 /* Private */,
       3,    0,   90,    2, 0x08 /* Private */,
       4,    0,   91,    2, 0x08 /* Private */,
       5,    1,   92,    2, 0x08 /* Private */,
       7,    0,   95,    2, 0x08 /* Private */,
       8,    2,   96,    2, 0x08 /* Private */,
      11,    0,  101,    2, 0x08 /* Private */,
      12,    0,  102,    2, 0x08 /* Private */,
      13,    0,  103,    2, 0x08 /* Private */,
      14,    0,  104,    2, 0x08 /* Private */,
      15,    0,  105,    2, 0x08 /* Private */,
      16,    0,  106,    2, 0x08 /* Private */,
      17,    0,  107,    2, 0x08 /* Private */,
      18,    0,  108,    2, 0x08 /* Private */,
      19,    0,  109,    2, 0x08 /* Private */,

 // slots: parameters
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void, QMetaType::Int,    6,
    QMetaType::Void,
    QMetaType::Void, QMetaType::Int, QMetaType::Int,    9,   10,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void gui::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        gui *_t = static_cast<gui *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->on_buttonNewGame_clicked(); break;
        case 1: _t->on_buttonBack_clicked(); break;
        case 2: _t->on_buttonLoadGame_clicked(); break;
        case 3: _t->on_comboBoxPlayer2_currentIndexChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: _t->on_pushButton_12_clicked(); break;
        case 5: _t->moveToPosition((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 6: _t->on_buttonNewGame2_clicked(); break;
        case 7: _t->on_buttonLoad2_clicked(); break;
        case 8: _t->on_buttonUndo_clicked(); break;
        case 9: _t->on_buttonRedo_clicked(); break;
        case 10: _t->on_buttonSave_clicked(); break;
        case 11: _t->on_buttonBackToGame_clicked(); break;
        case 12: _t->on_buttonChangeGame_clicked(); break;
        case 13: _t->on_buttonDemoGame_clicked(); break;
        case 14: _t->on_buttonPass_clicked(); break;
        default: ;
        }
    }
}

const QMetaObject gui::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_gui.data,
      qt_meta_data_gui,  qt_static_metacall, Q_NULLPTR, Q_NULLPTR}
};


const QMetaObject *gui::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *gui::qt_metacast(const char *_clname)
{
    if (!_clname) return Q_NULLPTR;
    if (!strcmp(_clname, qt_meta_stringdata_gui.stringdata0))
        return static_cast<void*>(const_cast< gui*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int gui::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 15)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 15;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 15)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 15;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
