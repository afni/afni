# Unix Makefile for niml functions

.c.o:
	$(CC) -c $<

NIML_OBJS = niml_b64.o        niml_element.o  niml_elemio.o                  niml_util.o  \
            niml_byteorder.o  niml_header.o   niml_malloc.o  niml_rowtype.o  niml_uuid.o   \
            niml_do.o         niml_htable.o   niml_md5.o     niml_url.o      niml_stream.o  \
            niml_struct.o     niml_dataset.o  niml_vector.o  niml_stat.o

all:$(NIML_OBJS)

clean:
	$(RM) *.o
